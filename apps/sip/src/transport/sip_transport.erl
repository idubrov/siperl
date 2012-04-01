%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Public API for the SIP transport layer. Provides functions to
%%% send SIP messages.
%%% Also implements fallback logic in case message sending fails.
%%% If message is too big, it retries sending the request with
%%% congestion-controlled protocol.
%%% If it fails for any other reason, it tries to send message on
%%% a new connection
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport).

-behaviour(gen_server).

%% Exports

%% API
-export([start_link/0]).
-export([send_request/3, send_response/1, is_reliable/1, monitor/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Internal API
-export([dispatch/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(state, {}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @doc Check if given transport is reliable.
%% @end
-spec is_reliable(atom()) -> boolean().
is_reliable(udp) -> false;
is_reliable(tcp) -> true.


-spec monitor(#sip_destination{}) -> ok.
%% @doc Start monitoring errors coming from given destination
%%
%% Process calling this function will receive messages in form of
%% `{transport_error, #sip_destination{}, Reason}' in case error
%% occurs while contacting to this destination (for example, destination port is unreachable).
%%
%% <em>Only errors that could not be reported synchronously are reported via
%% this mechanism (for example, ICMP messages coming from remote host after
%% sending UDP message)</em>.
%% @end
monitor(#sip_destination{address = Address, port = Port}) ->
    sip_transport_icmp:monitor(Address, Port).

%% @doc
%% Send request via the specified connection.
%% Opts = [term()]. The only supported option is {ttl, TTL} used
%% for sending multicast requests.
%% <em>Note: Content-Length header is automatically added if it is not present</em>
%% @end
-spec send_request(#sip_destination{}, #sip_request{}, [{ttl, non_neg_integer()}]) -> ok | {error, Reason :: term()}.
send_request(To, Request, Opts) when is_record(To, sip_destination) ->
    % Validate request
    ok = sip_message:validate_request(Request),

    % Add Content-Length, if missing
    Request2 = add_content_length(Request),

    % Update top Via header
    TTL = proplists:get_value(ttl, Opts, 1),
    Request3 = add_via_sentby(Request2, To, TTL),

    case transport_send(To, Request3) of
        ok -> ok;
        {error, too_big} ->
            % Try with congestion controlled protocol (TCP) (only for requests, 18.1)
            sip_log:too_big(Request3),

            % Change transport (note: using request without Via: updated)
            send_request(To#sip_destination{transport = tcp}, Request2, Opts);
        {error, Reason} -> {error, Reason}
     end.

%% @doc Send response by following RFC 3261 18.2.2
%% <em>Note: Content-Length header is automatically added if it is not present</em>
%% <em>Note: `To:' header `tag' parameter is automatically generated and added if it
%% is not present and status code is not 100</em>
%% @end
-spec send_response(#sip_response{}) -> ok | {error, Reason :: term()}.
send_response(Response) when is_record(Response, sip_response) ->
    % Validate response
    ok = sip_message:validate_response(Response),

    Response2 = add_content_length(Response),

    Via = sip_message:header_top_value(via, Response2),
    case is_reliable(Via#sip_hdr_via.transport) of
        true ->
            % try to lookup the connection
            Key = sip_transaction:tx_key(server, Response2),
            case gproc:lookup_pids({p, l, {connection, Key}}) of
                [Pid | _Rest] ->
                    Connection = #sip_connection{transport = Via#sip_hdr_via.transport, connection = Pid},
                    case transport_send(Connection, Response2) of
                        ok -> ok;
                        {error, _Reason} ->
                            % try to send to address in `received'
                            send_response_received(Response2)
                    end;
                [] ->
                    % no connections for tx, send to address `received'
                    send_response_received(Response2)
            end;
        false ->
            % not reliable -- send to address in `received'
            send_response_received(Response2)
    end.

%% @doc See RFC 3261 18.2.2 Sending Responses
%% @end
send_response_received(Response) ->
    Via = sip_message:header_top_value(via, Response),
    Params = Via#sip_hdr_via.params,
    Transport = Via#sip_hdr_via.transport,
    IsReliable = is_reliable(Transport),
    case lists:keyfind(maddr, 1, Params) of
        {_, MAddr} when not IsReliable ->
            % use 'maddr' parameter for unreliable transports
            Addr = sip_resolve:resolve(MAddr),
            Port = Via#sip_hdr_via.port,
            To = #sip_destination{address = Addr, port = Port, transport = Transport},
            transport_send(To, Response);
        false ->
            case lists:keyfind(received, 1, Params) of
                % use received parameter, must be IP
                {_, Received} ->
                    % if rport is present, send to it, RFC 3581 4
                    Port =
                        case lists:keyfind(rport, 1, Params) of
                            {rport, RPort} -> RPort;
                            false -> Via#sip_hdr_via.port
                        end,
                    To = #sip_destination{address = Received, port = Port, transport = Transport},
                    transport_send(To, Response);
                false ->
                    % Use procedures of Section 5 RFC 3263
                    Destinations = sip_resolve:server_resolve(Via),
                    % FIXME: TLS?
                    send_response_fallback(Destinations, Response)
            end
    end.

%% @doc Try sending response to destinations from the list
%% @end
send_response_fallback([], _Response) -> {error, no_backup_servers};
send_response_fallback([To|Rest], Response) ->
    case transport_send(To, Response) of
        ok -> ok;
        {error, _Reason} ->
            % try next destination
            send_response_fallback(Rest, Response)
    end.

%%-----------------------------------------------------------------
%% Internal transport API
%%-----------------------------------------------------------------

%% @doc Dispatch request/response, received by the transport socket.
%%
%% Dispatch request/response received from given destination. This function
%% is called by concrete transport implementations.
%% @end
%% @private
-spec dispatch(#sip_destination{}, sip_message()) -> ok.
dispatch(From, Request) when is_record(Request, sip_request) ->
    % pre-parse message
    Request2 = pre_parse(Request),

    Request3 = add_via_received(From, Request2),
    % 18.2.1: route to server transaction or to core
    case sip_transaction:handle_request(Request3) of
        not_handled -> dispatch_core(request, Request3);
        {ok, _TxRef} -> ok
    end,
    ok;
dispatch(From, Response) when is_record(Response, sip_response) ->
    % When a response is received, the client transport examines the top
    % Via header field value.  If the value of the "sent-by" parameter in
    % that header field value does not correspond to a value that the
    % client transport is configured to insert into requests, the response
    % MUST be silently discarded.
    case check_sent_by(From#sip_destination.transport, Response) of
        true ->
            % 18.1.2: route to client transaction or to core
            case sip_transaction:handle_response(Response) of
                not_handled -> dispatch_core(response, Response);
                {ok, _TxRef} -> ok
            end;
        {ExpectedSentBy, SentBy} ->
            sip_log:sentby_mismatch(Response, ExpectedSentBy, SentBy)
    end,
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
-spec add_via_sentby(sip_message(), #sip_destination{}, integer()) -> sip_message().
add_via_sentby(Message, #sip_destination{address = To, transport = Transport}, TTL) ->
    {Host, Port} = sent_by(Transport),
    Fun = fun (Via) ->
                   Params = Via#sip_hdr_via.params,
                   NewParams = case To of
                                   % multicast
                                   {A, _B, _C, _D} when A >= 224, A =< 239 -> % FIXME: utility...
                                       AddrBin = sip_syntax:format_addr(To),
                                       Params2 = lists:keystore('maddr', 1, Params,  {'maddr', AddrBin}),
                                       Params3 = lists:keystore('ttl', 1, Params2,  {'ttl', TTL}),
                                       Params3;

                                   _Other -> Params
                               end,
                   #sip_hdr_via{transport = Transport, host = Host, port = Port, params = NewParams}
          end,
    sip_message:update_top_header('via', Fun, Message).

-spec check_sent_by(atom(), sip_message()) -> true | {Expected :: term(), Actual :: term()}.
%% Check message sent by matches one inserted by the transport layer
check_sent_by(Transport, Msg) ->
    % port is explicit in expected sent-by
    ExpectedSentBy = sent_by(Transport),

    % take top via sent-by
    Via = sip_message:header_top_value('via', Msg),
    case Via#sip_hdr_via.port of
        % Default port, RFC 3261 18.1
        undefined ->
            SentBy = {Via#sip_hdr_via.host, default_port(Transport)};
        Port ->
            SentBy = {Via#sip_hdr_via.host, Port}
    end,
    if
        SentBy =:= ExpectedSentBy -> true;
        true -> {ExpectedSentBy, SentBy}
    end.

% If the host portion of the "sent-by" parameter contains a domain name, or if
% it contains an IP address that differs from the packet source address, the
% server MUST add a "received" parameter to that Via header field value.
% RFC 3261, 18.2.1

add_via_received(#sip_destination{address = Src, port = Port}, Msg) when is_tuple(Src) ->
    % Note: according to the RFC 3581 4, we always add 'received' parameter
    Fun =
        fun (TopVia) ->
                 Params = lists:keystore(received, 1, TopVia#sip_hdr_via.params, {received, Src}),
                 UpdateRPort =
                     fun(rport) -> {rport, Port};
                        (Value) -> Value
                     end,
                 Params2 = lists:map(UpdateRPort, Params),
                 TopVia#sip_hdr_via{params = Params2}
        end,
    sip_message:update_top_header(via, Fun, Msg).

%% @doc
%% Default transports ports
%% @end
default_port(udp) -> 5060;
default_port(tcp) -> 5060.

%% @doc Get the value for the via sent-by.
%% @end
sent_by(Transport) ->
    gen_server:call(?SERVER, {get_sentby, Transport}).

%% @doc Send the message through the transport.
%% @end
transport_send(#sip_destination{port = undefined} = To, Message) ->
    DefaultPort = default_port(To#sip_destination.transport),
    transport_send(To#sip_destination{port = DefaultPort}, Message);
transport_send(To, Message) when is_record(To, sip_destination)->
    Module = transport_module(To#sip_destination.transport),
    Module:send(To, Message);
transport_send(To, Message) when is_record(To, sip_connection) ->
    Module = transport_module(To#sip_connection.transport),
    Module:send(To, Message).

transport_module(udp) -> sip_transport_udp;
transport_module(tcp) -> sip_transport_tcp.

%% @doc Dispatch SIP message to the core that will handle it
%% @end
dispatch_core(Kind, Msg) ->
    case sip_cores:lookup_core(Msg) of
        {ok, Pid, _Reg} ->
            % Note: according to RFC 6026 responses MUST be dropped by any element
            % other than stateless proxy
            Pid ! {Kind, Msg},
            ok;
        undefined ->
            sip_log:no_core(Msg),
            ok
    end.

%% @doc Parse required headers
%% @end
pre_parse(#sip_request{} = Msg) ->
    URI = sip_uri:parse(Msg#sip_request.uri),
    Headers = pre_parse_headers(Msg#sip_request.headers),
    Msg#sip_request{uri = URI, headers = Headers}.

pre_parse_headers(Headers) ->
    % TODO: Catch incorrect headers and report as bad request/response
    Fun =
        fun({Name, Value}) when
             Name =:= to; Name =:= from; Name =:= cseq; Name =:= 'call-id';
             Name =:= 'max-forwards'; Name =:= via; Name =:= 'contact' ->
                {Name, sip_headers:parse(Name, Value)};
           (Other) -> Other
        end,
    lists:map(Fun, Headers).

%% Add Content-Length to the message, if not present
add_content_length(Msg) ->
    Fun =
        fun(undefined) ->
                case Msg of
                    #sip_request{body = Body} -> size(Body);
                    #sip_response{body = Body} -> size(Body)
                end;
           (Value) -> Value
        end,
    sip_message:update_top_header('content-length', Fun, Msg).

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
    {Host, undefined, <<>>} = sip_syntax:parse_host_port(Self),
    [Port | _] = sip_config:ports(Transport),
    {reply, {Host, Port}, State};
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
