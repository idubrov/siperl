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
-export([send_request/3, send_response/2, is_reliable/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Internal API
-export([dispatch_request/3, dispatch_response/3]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

%% Types
-opaque connection() :: pid() | term().
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
-spec is_reliable(atom()) -> boolean().
is_reliable(udp) -> false;
is_reliable(tcp) -> true.

%% @doc
%% Send request via the specified connection.
%% Opts = [term()]. The only supported option is {ttl, TTL} used
%% for sending multicast requests.
%% @end
-spec send_request(#sip_destination{}, #sip_message{}, [term()]) ->
          {ok, connection()} | {error, Reason :: term()}.
send_request(To, Request, Opts) when is_record(To, sip_destination) ->
    % Validate request
    true = sip_message:is_request(Request),
    ok = sip_message:validate_request(Request),

    % Update top Via header
    TTL = proplists:get_value(ttl, Opts, 1),
    Request2 = add_via_sentby(Request, To, TTL),

    case transport_send(To#sip_destination.transport, To, Request2) of
        {error, too_big} ->
            % Try with congestion controlled protocol (TCP) (only for requests, 18.1)
            error_logger:warning_msg("Message is too big, re-sending using TCP"),

            % Change transport
            send_request(To#sip_destination{transport = tcp}, Request, Opts);
        Result -> Result
     end.

%% @doc
%% Try to send reply by following RFC 3261 18.2.2.
%% @end
-spec send_response(connection() | undefined, #sip_message{}) -> {ok, connection()} | {error, Reason :: term()}.
send_response(undefined, Response) ->
    To = reply_address(Response),
    send_response(To, Response);
send_response(Connection, Response) ->
    % Validate response
    true = sip_message:is_response(Response),

    % Determine transport to send response with
    {ok, Via} = sip_message:top_header('via', Response),
    Transport = Via#sip_hdr_via.transport,
    try transport_send(Transport, Connection, Response)
    catch exit:{noproc, _Reason} ->
              % Try to send again to the address in "received" and port in sent-by
              send_response(undefined, Response)
    end.

%%-----------------------------------------------------------------
%% Internal transport API
%%-----------------------------------------------------------------

%% @doc
%% Dispatch request received through given connection. This function
%% is called by concrete transport implementations.
%% @end
%% @private
-spec dispatch_request(#sip_destination{}, connection(), #sip_message{}) -> ok.
dispatch_request(From, Connection, Msg) ->
    Msg2 = add_via_received(From, Msg),
    % 18.1.2: route to client transaction or to core
    case sip_transaction:handle_request(Msg2) of
        not_handled -> sip_core:handle_request(From, Connection, Msg2);
        {ok, _TxRef} -> ok
    end.

%% @doc
%% Dispatch response received through given connection. This function
%% is called by concrete transport implementations.
%% @end
%% @private
-spec dispatch_response(#sip_destination{}, connection(), #sip_message{}) -> ok.
dispatch_response(From, Connection, Msg) ->
    % When a response is received, the client transport examines the top
    % Via header field value.  If the value of the "sent-by" parameter in
    % that header field value does not correspond to a value that the
    % client transport is configured to insert into requests, the response
    % MUST be silently discarded.
    case check_sent_by(From#sip_destination.transport, Msg) of
        true ->
            % 18.2.1: route to server transaction or to core
            case sip_transaction:handle_response(Msg) of
                not_handled -> sip_core:handle_response(From, Connection, Msg);
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
-spec add_via_sentby(#sip_message{}, #sip_destination{}, integer()) -> #sip_message{}.
add_via_sentby(Message, #sip_destination{address = Addr, transport = Transport}, TTL) ->
    SentBy = sent_by(Transport),
    % XXX: Only ipv4 for now.
    {ok, To} = inet:getaddr(Addr, inet),

    Fun = fun ('via', Via) ->
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
                   #sip_hdr_via{transport = Transport, sent_by = SentBy, params = NewParams}
          end,
    sip_message:update_top_header('via', Fun, Message).

-spec check_sent_by(atom(), #sip_message{}) -> true | {Expected :: term(), Actual :: term()}.
%% Check message sent by matches one inserted by the transport layer
check_sent_by(Transport, Msg) ->
    {Addr, Port} = sent_by(Transport),
    ExpectedSentBy = {sip_binary:any_to_binary(Addr), Port},

    % take top via sent-by
    {ok, Via} = sip_message:top_header('via', Msg),
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
add_via_received(#sip_destination{address = Src}, Msg) ->
    Fun = fun ('via', TopVia) ->
                   % compare byte-to-byte with packet source address
                   SrcBin = sip_binary:any_to_binary(Src),
                   case TopVia#sip_hdr_via.sent_by of
                       {SrcBin, _} ->
                           TopVia;

                       _ ->
                           Params = lists:keystore('received', 1, TopVia#sip_hdr_via.params,
                                                   {'received', SrcBin}),
                           TopVia#sip_hdr_via{params = Params}
                   end
          end,
    sip_message:update_top_header('via', Fun, Msg).

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
reply_address(Message) ->
    {ok, Via} = sip_message:top_header('via', Message),
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
    #sip_destination{address = Addr,
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
transport_send(udp, To, Message) -> sip_transport_udp:send(To, Message);
transport_send(tcp, To, Message) -> sip_transport_tcp:send(To, Message).

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
