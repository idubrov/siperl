%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Public API for the SIP transport layer. Provides functions to
%%% send SIP messages.
%%% Also implements fallback logic in case message sending fails.
%%% If message is too big, it retries sending the request with
%%% congestion-controlled protocol.
%%% If it fails for any other reason, it tries to send message on
%%% a new connection
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport).

-behaviour(gen_server).

%% Exports

%% API
-export([start_link/0]).
-export([send_request/3, send_request/4, send_response/2, is_reliable/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Internal API
-export([dispatch/3]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_message.hrl").

%% Types
-opaque proc() :: term().

-type connection() :: {Remote :: #conn_idx{}, Proc :: proc()} | #conn_idx{}.
-export_type([connection/0]).

-record(state, {}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @doc
%% Check if given transport is reliable
%% @end
-spec is_reliable(atom()) -> boolean().
is_reliable(udp) -> false;
is_reliable(tcp) -> true.

%% @doc
%% Send message to the specified endpoint. This function returns
%% connection that should be used to send subsequent messages via send/3.
%% @end
-spec send_request(#conn_idx{}, #sip_message{}, [term()]) -> {ok, connection()} | {error, Reason :: term()}.
send_request(To, Message, Opts) ->
    send_request(undefined, To, Message, Opts).

%% @doc
%% Send packet via the specified connection. Returns connection that
%% should be used for subsequent send/3 calls.
%% @end
-spec send_request(connection() | undefined, #conn_idx{}, #sip_message{}, integer()) ->
          {ok, connection()} | {error, Reason :: term()}.
send_request(undefined, To, Message, Opts) ->
    {ok, Endpoint} = endpoint(To),
    send_request(Endpoint, To, Message, Opts);

send_request(Endpoint, To, Message, Opts) when
  is_record(To, conn_idx),
  is_record(Message, sip_message) ->
    true = sip_message:is_request(Message),

    % Update top Via header
    TTL = proplists:get_value(ttl, Opts, 1),
    NewMessage = add_via_sentby(Message, sent_by(To#conn_idx.transport), To, TTL),

     case send(Endpoint, To#conn_idx.transport, NewMessage) of
        {error, too_big} ->
            % Try with congestion controlled protocol (TCP) (only for requests, 18.1)
            error_logger:warning_msg("Message is too big, re-sending using TCP"),
            send_request(To#conn_idx{transport = tcp}, Message, Opts);

        Result -> Result
     end.

%% @doc
%% Try to send reply by following RFC 3261 18.2.2. Connection is either opaque
%% value returned by request/response or 'false'.
%% @end
-spec send_response(connection() | undefined, #sip_message{}) -> {ok, connection()}.
%% Reply to address specified in 'received' parameter
send_response(undefined, Message) when is_record(Message, sip_message) ->
    true = sip_message:is_response(Message),

    Via = sip_headers:top_via(Message#sip_message.headers),
    To = reply_address(Via),
    {ok, Endpoint} = endpoint(To),
    send_response(Endpoint, Message);

%% Reply on given connection
send_response(Endpoint, Message) when is_record(Message, sip_message) ->
    true = sip_message:is_response(Message),

    Via = sip_headers:top_via(Message#sip_message.headers),
    try send(Endpoint, Via#sip_hdr_via.transport, Message)
    catch exit:{noproc, _Reason} ->
              % Try to send again to the address in "received" and port in sent-by
              send_response(undefined, Message)
    end.

%%-----------------------------------------------------------------
%% Internal transport API
%%-----------------------------------------------------------------

%% @doc
%% Dispatch message received through given connection. This function
%% is called by concrete transport implementations.
%% @end
%% @private
-spec dispatch(term(), #conn_idx{}, #sip_message{} | [#sip_message{}]) -> ok.
dispatch(Connection, Remote, Msgs) when is_list(Msgs) ->
    lists:foreach(fun (Msg) -> dispatch(Connection, Remote, Msg) end, Msgs);

dispatch(Connection, Remote, Msg)
  when is_record(Remote, conn_idx),
       is_record(Msg, sip_message) ->

    case sip_message:is_request(Msg) of
        true -> dispatch_request(Connection, Remote, Msg);
        false -> dispatch_response(Connection, Remote, Msg)
    end.

dispatch_request(Connection, From, Msg) ->
    Msg2 = add_via_received(From, Msg),
    % 18.1.2: route to client transaction or to core
    case sip_transaction:handle(Connection, From, Msg2) of
        not_handled -> sip_core:handle(Connection, From, Msg2);
        {ok, _TxRef} -> ok
    end.

dispatch_response(Connection, From, Msg) ->
    % When a response is received, the client transport examines the top
    % Via header field value.  If the value of the "sent-by" parameter in
    % that header field value does not correspond to a value that the
    % client transport is configured to insert into requests, the response
    % MUST be silently discarded.
    case check_sent_by(From#conn_idx.transport, Msg) of
        true ->
            % 18.2.1: route to server transaction or to core
            case sip_transaction:handle(Connection, From, Msg) of
                not_handled -> sip_core:handle(Connection, From, Msg);
                {ok, _TxRef} -> ok
            end;

        {ExpectedSentBy, SentBy} ->
            error_logger:warning_report(['message_discarded',
                                         {'expected_sent_by', ExpectedSentBy},
                                         {'sent_by', SentBy},
                                         {msg, Msg}])
    end.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
-spec add_via_sentby(#sip_message{}, SentBy :: {binary(), integer() | undefined}, #conn_idx{}, integer()) ->
          #sip_message{}.
add_via_sentby(Message, SentBy, #conn_idx{address = Addr, transport = Transport}, TTL) ->
    % XXX: Only ipv4 for now.
    {ok, To} = inet:getaddr(Addr, inet),

    Fun = fun ('via', [Via | Rest]) ->
                   Params = Via#sip_hdr_via.params,
                   NewParams = case To of
                                   % multicast
                                   {A, _B, _C, _D} when A >= 224, A =< 239 -> % FIXME: utility...
                                       AddrBin = sip_binary:any_to_binary(To),
                                       Params2 = lists:keystore('maddr', 1, Params,  {'maddr', AddrBin}),
                                       Params3 = lists:keystore('ttl', 1, Params2,  {'ttl', TTL}),
                                       Params3;

                                   _Other -> Params
                               end,
                   NewVia = Via#sip_hdr_via{transport = Transport,
                                            sent_by = SentBy,
                                            params = NewParams},
                   [NewVia | Rest]
          end,
    NewHeaders = sip_headers:update_top_header('via', Fun, Message#sip_message.headers),
    Message#sip_message{headers = NewHeaders}.

-spec check_sent_by(atom(), #sip_message{}) -> true | {Expected :: term(), Actual :: term()}.
%% Check message sent by matches one inserted by the transport layer
check_sent_by(Transport, Msg) ->
    {Addr, Port} = sent_by(Transport),
    ExpectedSentBy = {sip_binary:any_to_binary(Addr), Port},

    % take top via sent-by
    Via = sip_headers:top_via(Msg#sip_message.headers),
    SentBy = Via#sip_hdr_via.sent_by,
    ActualSentBy =
        case SentBy of
            % Default port, RFC 3261 18.1
            {SentByAddr, undefined} ->
                {SentByAddr, default_port(Transport)};
            _ ->
                SentBy
        end,
    if
        ActualSentBy =:= ExpectedSentBy -> true;
        true -> {ExpectedSentBy, ActualSentBy}
    end.

% If the host portion of the "sent-by" parameter contains a domain name, or if
% it contains an IP address that differs from the packet source address, the
% server MUST add a "received" parameter to that Via header field value.
% RFC 3261, 18.2.1
add_via_received(#conn_idx{address = Src}, Msg) ->
    Fun = fun ('via', Value) ->
                   {'via', [TopVia|Rest]} = sip_headers:parse_header('via', Value),

                   % compare byte-to-byte
                   SrcBin = sip_binary:any_to_binary(Src),
                   case TopVia#sip_hdr_via.sent_by of
                       {SrcBin, _} ->
                           [TopVia|Rest];

                       _ ->
                           Params = lists:keystore('received', 1, TopVia#sip_hdr_via.params,
                                                   {'received', SrcBin}),
                           TopVia2 = TopVia#sip_hdr_via{params = Params},
                           [TopVia2|Rest]
                   end
          end,
    Headers = sip_headers:update_top_header('via', Fun, Msg#sip_message.headers),
    Msg#sip_message{headers = Headers}.

to_address(List) when is_list(List) ->
    List;

to_address(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);

to_address({A, B, C, D} = Addr) when
  is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    Addr.

%% @doc
%% See RFC 3261 18.2.2 Sending Responses
%% @end
reply_address(Via) ->
    Transport = Via#sip_hdr_via.transport,
    IsReliable = is_reliable(Transport),
    Addr =
        case lists:keyfind('maddr', 1, Via#sip_hdr_via.params) of
            {_, MAddr} when not IsReliable ->
                to_address(MAddr); % use 'maddr' parameter for unreliable transports

            false ->
                case lists:keyfind('received', 1, Via#sip_hdr_via.params) of
                    {_, Received} ->
                        to_address(Received); % use 'received' parameter

                    false ->
                        SentBy = element(1, Via#sip_hdr_via.sent_by),
                        to_address(SentBy) % use 'sent-by' parameter
                end
        end,
    Port = case Via#sip_hdr_via.sent_by of
               {_, undefined} -> default_port(Transport);
               {_, P} -> P
           end,
    #conn_idx{address = Addr,
                  port = Port,
                  transport = Transport}.


%% @doc
%% Default transports ports
%% @end
default_port(udp) -> 5060;
default_port(tcp) -> 5060.

%% @doc
%% Get the value for the via sent-by.
%% @end
sent_by(Transport) ->
    gen_server:call(?SERVER, {get_sentby, Transport}).

%% @doc
%% Send the message through the transport.
%% @end
send(Endpoint, udp, Message) -> sip_transport_udp:send(Endpoint, Message);
send(Endpoint, tcp, Message) -> sip_transport_tcp:send(Endpoint, Message).

%% @doc
%% Lookup endpoint to send message through for the transport.
%% @end
-spec endpoint(#conn_idx{}) -> {ok, connection()}.
endpoint(To) when To#conn_idx.transport =:= udp -> sip_transport_udp:connect(To);
endpoint(To) when To#conn_idx.transport =:= tcp -> sip_transport_tcp:connect(To).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_call(_, _, #state{}) -> {reply, atom(), #state{}} | {stop, _, #state{}}.
handle_call({get_sentby, Transport}, _From, State) ->
    Self = sip_config:self(),
    [Port | _] = sip_config:ports(Transport),
    {reply, {Self, Port}, State};
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_cast(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
