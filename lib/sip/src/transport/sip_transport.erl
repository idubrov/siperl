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
-export([connection/3]).
-export([send_request/2, send_request/3, send_response/2, is_reliable/1]).

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
-opaque connection() :: {Remote :: #conn_key{}, Proc :: pid() | undefined}.
-export_type([connection/0]).

-record(state, {}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @doc
%% Check if given transport is reliable.
%% @end
-spec is_reliable(atom() | connection()) -> boolean().
is_reliable(#conn_key{transport = Transport}) ->
    is_reliable(Transport);
is_reliable({#conn_key{transport = Transport}, _ConnProc}) ->
    is_reliable(Transport);
is_reliable(udp) -> false;
is_reliable(tcp) -> true.

%% @doc
%% Create connection to the given address/port via specified transport.
%% @end
-spec connection(term(), integer(), atom()) -> connection().
connection(Address, Port, Transport) ->
    {#conn_key{address = Address, port = Port, transport = Transport}, undefined}.

%% @doc
%% Send packet via the specified connection. Returns connection that
%% should be used for subsequent send_* calls.
%% @end
-spec send_request(connection() | #conn_key{}, #sip_message{}) ->
          {ok, connection()} | {error, Reason :: term()}.
send_request(Connection, Message) ->
    send_request(Connection, Message, []).

%% @doc
%% Send packet via the specified connection. Returns connection that
%% should be used for subsequent send_* calls.
%% Opts = [term()]. The only supported option is {ttl, TTL} used
%% for sending multicast requests.
%% @end
-spec send_request(connection(), #sip_message{}, [term()]) ->
          {ok, connection()} | {error, Reason :: term()}.
send_request({ConnKey, ConnProc}, Message, Opts) when
  is_record(ConnKey, conn_key),
  is_record(Message, sip_message) ->
    true = sip_message:is_request(Message),

    % Update top Via header
    TTL = proplists:get_value(ttl, Opts, 1),
    NewMessage = add_via_sentby(Message, ConnKey, TTL),

     case send(ConnKey, ConnProc, NewMessage) of
        {error, too_big} ->
            % Try with congestion controlled protocol (TCP) (only for requests, 18.1)
            error_logger:warning_msg("Message is too big, re-sending using TCP"),
            
            ConnKey2 = ConnKey#conn_key{transport = tcp},
            send_request(connection(ConnKey2, undefined), Message, Opts);

        {ok, ConnProc2} -> {ok, {ConnKey, ConnProc2}};
        {error, Reason} -> {error, Reason}
     end.

%% @doc
%% Try to send reply by following RFC 3261 18.2.2.
%% @end
-spec send_response(undefined | connection(), #sip_message{}) -> {ok, connection()} | {error, Reason :: term()}.
send_response({_ConnKey, ConnProc}, Message) ->
    send_response(ConnProc, Message);
send_response(ConnProc, Message)
  when (is_pid(ConnProc) orelse ConnProc =:= undefined), is_record(Message, sip_message) ->
    true = sip_message:is_response(Message),

    Via = sip_headers:top_via(Message#sip_message.headers),
    To = reply_address(Via),
    Result = 
        try send(To, ConnProc, Message)
        catch exit:{noproc, _Reason} ->
                  % Try to send again to the address in "received" and port in sent-by
                  send_response(connection(To, undefined), Message)
        end,
    case Result of
        {ok, ConnProc2} -> {ok, {To, ConnProc2}};
        {error, Reason} -> {error, Reason}
    end.

%%-----------------------------------------------------------------
%% Internal transport API
%%-----------------------------------------------------------------

%% @doc
%% Dispatch message received through given connection. This function
%% is called by concrete transport implementations.
%% @end
%% @private
-spec dispatch(#conn_key{}, pid(), #sip_message{} | [#sip_message{}]) -> ok.
dispatch(Remote, ConnProc, Msgs) when is_list(Msgs) ->
    lists:foreach(fun (Msg) -> dispatch(Remote, ConnProc, Msg) end, Msgs);

dispatch(Remote, ConnProc, Msg)
  when is_record(Remote, conn_key),
       is_pid(ConnProc),
       is_record(Msg, sip_message) ->

    Connection = connection(Remote, ConnProc),
    case sip_message:is_request(Msg) of
        true -> dispatch_request(Connection, Msg);
        false -> dispatch_response(Connection, Msg)
    end.

-spec dispatch_request(connection(), #sip_message{}) -> ok.
dispatch_request({From, _ConnProc} = Connection, Msg) ->
    Msg2 = add_via_received(From, Msg),
    % 18.1.2: route to client transaction or to core
    case sip_transaction:handle(Connection, Msg2) of
        not_handled -> sip_core:handle(Connection, Msg2);
        {ok, _TxRef} -> ok
    end.

dispatch_response({From, _ConnProc} = Connection, Msg) ->
    % When a response is received, the client transport examines the top
    % Via header field value.  If the value of the "sent-by" parameter in
    % that header field value does not correspond to a value that the
    % client transport is configured to insert into requests, the response
    % MUST be silently discarded.
    case check_sent_by(From#conn_key.transport, Msg) of
        true ->
            % 18.2.1: route to server transaction or to core
            case sip_transaction:handle(Connection, Msg) of
                not_handled -> sip_core:handle(Connection, Msg);
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
-spec add_via_sentby(#sip_message{}, #conn_key{}, integer()) -> #sip_message{}.
add_via_sentby(Message, #conn_key{address = Addr, transport = Transport}, TTL) ->
    SentBy = sent_by(Transport),
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
add_via_received(#conn_key{address = Src}, Msg) ->
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
    #conn_key{address = Addr,
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
send(#conn_key{transport = udp} = ConnKey, ConnProc, Message) -> sip_transport_udp:send(ConnKey, ConnProc, Message);
send(#conn_key{transport = tcp} = ConnKey, ConnProc, Message) -> sip_transport_tcp:send(ConnKey, ConnProc, Message).

%% @doc
%% Internal function to create opaque connection() terms.
%% @end
-spec connection(#conn_key{}, pid() | undefined) -> connection().
connection(ConnKey, ConnProc) ->
    {ConnKey, ConnProc}.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_call(_, _, #state{}) -> {reply, {term(), integer()}, #state{}} | {stop, _, #state{}}.
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
