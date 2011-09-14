%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Transport layer TCP connection implementation
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport_tcp_conn).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([send/2]).

%% Server callbacks
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {socket, remote, parse_state = none, timeout}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
-spec start_link(#sip_destination{} | inet:socket()) -> {ok, pid()}.
start_link(Remote) ->
    gen_server:start_link(?MODULE, Remote, []).

%% @doc Send SIP message through given connection.
%% @end
-spec send(pid(), sip_message()) -> ok | {error, Reason :: term()}.
send(Pid, Message) when
  is_pid(Pid),
  (is_record(Message, sip_request) orelse is_record(Message, sip_response)) ->
    try gen_server:call(Pid, {send, Message})
    catch exit:{noproc, _Reason} -> {error, not_connected}
    end.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(inet:socket() | #sip_destination{}) -> {ok, #state{}, integer()} | {stop, term()}.
init(Remote)
  when is_record(Remote, sip_destination) ->
    % New connection
    To = Remote#sip_destination.address,
    Port = Remote#sip_destination.port,
    case gen_tcp:connect(To, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            init(Socket);
        {error, Reason} ->
            {stop, Reason}
    end;
init(Socket) ->
    % RFC3261 Section 18
    % These connections are indexed by the tuple formed from the address,
    % port, and transport protocol at the far end of the connection.
    {ok, {RemoteAddress, RemotePort}} = inet:peername(Socket),
    Remote = #sip_destination{transport = tcp, address = RemoteAddress, port = RemotePort},

    Timeout = sip_config:connection_timeout(),

    % Enable one time delivery
    ok = inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket = Socket, remote = Remote, timeout = Timeout}, Timeout}.

%% @private
-spec handle_info({tcp, inet:socket(), binary()} | tcp_closed | term(), #state{}) ->
          {noreply, #state{}, integer()} | {stop, normal, #state{}} | {stop, {unexpected, _}, #state{}}.
handle_info({tcp, _Socket, Packet}, State) ->
    {ok, NewState} = process_stream(Packet, State),

    ok = inet:setopts(NewState#state.socket, [{active, once}]),
    {noreply, NewState, State#state.timeout};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_call({send, sip_message()}, _, #state{}) ->
          {reply, ok | {error, Reason :: term()}, #state{}, integer()} | {stop, {unexpected, _}, #state{}}.
handle_call({send, Message}, _From, State) ->
    Socket = State#state.socket,
    Packet = sip_message:to_binary(Message),
    Result = gen_tcp:send(Socket, Packet),
    {reply, Result, State, State#state.timeout};
handle_call(Req, _From, State) ->
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

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
process_stream(Packet, State) ->
    case sip_message:parse_stream(Packet, State#state.parse_state) of
        {ok, NewParseState} ->
            % no more messages to parse -- return
            {ok, State#state{parse_state = NewParseState}};
        {ok, Msg, NewParseState} ->
            case validate(Msg) of
                ok ->
                    process_message(State, Msg);
                {error, Reason} ->
                    bad_message(Msg, Reason, State)
            end,

            % there could be more messages to parse, recurse
            process_stream(<<>>, State#state{parse_state = NewParseState});
        {error, Reason, Msg, NewParseState} ->
            bad_message(Msg, Reason, State),

            % there could be more messages to parse, recurse
            process_stream(<<>>, State#state{parse_state = NewParseState})
    end.

process_message(State, Msg) ->
    % add property to register with gproc for requests
    % each property is a marker that this process handles
    % given server transaction, so we could find proper
    % connection when sending responses
    TxKey = sip_transaction:tx_key(server, Msg),
    gproc:add_local_property({connection, TxKey}, true),

    % dispatch to transport layer
    ok = sip_transport:dispatch(State#state.remote, Msg).

validate(#sip_request{} = Request) -> sip_message:validate_request(Request);
validate(#sip_response{} = Response) -> sip_message:validate_response(Response).

bad_message(#sip_request{} = Request, Reason, State) ->
    sip_log:bad_request(Request, Reason, State#state.remote),

    % TODO: Put reason in the response
    Response = sip_message:create_response(Request, 400),
    Packet = sip_message:to_binary(Response),

    % Don't bother about return value
    _Ignore = gen_tcp:send(State#state.socket, Packet),
    ok;
bad_message(#sip_response{} = Response, Reason, State) ->
    sip_log:bad_response(Response, Reason, State#state.remote),
    ok.

