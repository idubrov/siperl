%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc UDP SIP socket implementation.
%%%
%%% This module could operate in two modes: listening UDP process and
%%% "connected" UDP process.
%%%
%%% When started without destination address, process is started in
%%% listening mode. In this mode process accepts all messages on given
%%% port.
%%%
%%% When started with destination address, process is started in
%%% "connected" mode. This mode differs from regular mode in the
%%% following:
%%% <ul>
%%% <li>UDP socket created uses ephemeral port. Note that according
%%% to the RFC 3261 18.1.1 responses are sent to the port in Via: sent-by,
%%% therefore, in general, in this mode process will not receive any
%%% UDP messages.</li>
%%% <li>When started, process connects to the destination address
%%% via `gen_udp:connect/3'. This allows receiving subsequent ICMP
%%% errors.</li>
%%% <li>When started, process registers via `gproc' under
%%% {udp, RemoteAddr, RemotePort} local name.</li>
%%% <li>If no request or message is received within configured timeout,
%%% process terminates.</li>
%%% </ul>
%%% This mode is suitable for communicating with the remote party.
%%%
%%% <em>Since this process register itself with `gproc' in "connected"
%%% mode, the same process should be used by all parties communicating
%%% with same remote address</em>
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------,
-module(sip_transport_udp_socket).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([send/3]).

%% Server callbacks
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

% Keep socket for 32 seconds after last event
-define(TIMEOUT, 32000).

-record(state, {socket, error, timeout = infinity :: integer() | infinity}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------


%% @private
%% @doc
%% Send SIP message through the given socket.
%% @end
-spec send(pid(), #sip_destination{}, #sip_message{}) -> ok | {error, Reason :: term()}.
send(Pid, To, Message) when is_pid(Pid),
                                 is_record(To, sip_destination),
                                 is_record(Message, sip_message) ->
    gen_server:call(Pid, {send, To, Message}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec start_link(integer() | #sip_destination{}) -> {ok, pid()} | ignore | {error, term()}.
start_link(Param) when is_integer(Param); is_record(Param, sip_destination) ->
    gen_server:start_link(?MODULE, Param, []).

%% @private
-spec init(integer() | #sip_destination{}) -> {ok, #state{}}.
init(Port) when is_integer(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, inet, {reuseaddr, true}]),
    {ok, #state{socket = Socket}};
init(#sip_destination{address = ToAddr, port = ToPort}) ->
    {ok, Socket} = gen_udp:open(0, [binary, inet, {reuseaddr, true}]),

    % connect and register local name, so it could be reused by another client
    ok = gen_udp:connect(Socket, ToAddr, ToPort),
    gproc:add_local_name({udp, ToAddr, ToPort}),

    % "connected" should terminate after timeout to free resources
    {ok, #state{socket = Socket, timeout = ?TIMEOUT}, ?TIMEOUT}.

%% @private
-spec handle_info({udp, inet:socket(), inet:address(), integer(), binary()} | term(), #state{}) ->
          {noreply, #state{}, infinity} | {stop, _Reason, #state{}}.
handle_info({udp, Socket, SrcAddress, SrcPort, Packet}, State) ->
    Remote = #sip_destination{transport = udp, address = SrcAddress, port = SrcPort},
    Result = sip_message:parse_datagram(Packet),
    % result is either {ok, Msg} or {error, Reason, Msg}
    case sip_transport:dispatch(Remote, #sip_connection{transport = udp}, Result) of
        ok -> ok;
        {reply, Response} ->
            % we need to reply immediately
            % this functionality is used for sending responses on bad requests
            % so the sip_transport:dispatch/3 does not block on regular
            % `sip_transport_udp:send/2' call
            Packet = sip_message:to_binary(Response),
            gen_udp:send(Socket, SrcAddress, SrcPort, Packet)
    end,
    {noreply, State, State#state.timeout};
handle_info({udp_error, _Socket, Reason}, State) ->
    {noreply, State#state{error = Reason}, State#state.timeout};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_call({send, #sip_destination{}, #sip_message{}}, _, #state{}) ->
          {reply, {error, too_big}, #state{}, integer() | infinity} |
          {reply, {ok, #sip_destination{}}, #state{}, integer() | infinity}.
handle_call(_Req, _From, #state{error = Reason} = State)
  when Reason =/= undefined ->
    {reply, {error, Reason}, State#state{error = undefined}, State#state.timeout};
handle_call({send, To, Message}, _From, State) ->
    Packet = sip_message:to_binary(Message),
    %% If a request is within 200 bytes of the path MTU, or if it is larger
    %% than 1300 bytes and the path MTU is unknown, the request MUST be sent
    %% using an RFC 2914 [43] congestion controlled transport protocol, such
    %% as TCP.
    Reply =
        if
            size(Packet) > 1300 ->
                {error, too_big};
            true ->
                gen_udp:send(State#state.socket, To#sip_destination.address, To#sip_destination.port, Packet)
        end,
    {reply, Reply, State, State#state.timeout};

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

