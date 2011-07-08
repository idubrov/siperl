%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% UDP SIP socket implementation. This module provides both listener
%%% and sender implementation.
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
-include_lib("sip_transport.hrl").
-include_lib("sip_message.hrl").


-record(state, {socket}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------


%% @private
%% @doc
%% Send SIP message through the given socket.
%% @end
-spec send(pid(), #conn_idx{}, #sip_message{}) -> {ok, {pid(), #conn_idx{}}} | {error, Reason :: term()}.
send(Pid, To, Message) when is_pid(Pid),
                                 is_record(To, conn_idx),
                                 is_record(Message, sip_message) ->
    gen_server:call(Pid, {send, To, Message}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Port) when is_integer(Port) ->
inet:getif(),
    gen_server:start_link(?MODULE, Port, []).

%% @private
-spec init(integer()) -> {ok, #state{}}.
init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, inet]),
    {ok, #state{socket = Socket}}.

%% @private
%% When the connection is accepted by the transport layer, this
%% index is set to the source IP address, port number, and transport.
-spec handle_info({udp, inet:socket(), inet:address(), integer(), binary()} | term(), #state{}) ->
          {noreply, #state{}} | {stop, {unexpected, _}, #state{}}.
handle_info({udp, _Socket, SrcAddress, SrcPort, Packet}, State) ->
    RemoteEndpoint = #conn_idx{transport = udp, address = SrcAddress, port = SrcPort},
    {ok, Msg} = sip_message:parse_datagram(Packet),
    sip_transport:dispatch(self(), RemoteEndpoint, Msg),
    {noreply, State};

handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_call({send, #conn_idx{}, #sip_message{}}, _, #state{}) ->
          {reply, {error, too_big}, #state{}} |
          {reply, {ok, {pid(), #conn_idx{}}}, #state{}}.
handle_call({send, To, Message}, _From, State) ->
    Packet = sip_message:to_binary(Message),
    %% If a request is within 200 bytes of the path MTU, or if it is larger
    %% than 1300 bytes and the path MTU is unknown, the request MUST be sent
    %% using an RFC 2914 [43] congestion controlled transport protocol, such
    %% as TCP.
    if
        size(Packet) > 1300 ->
            {reply, {error, too_big}, State};
        true ->
            ok = gen_udp:send(State#state.socket, To#conn_idx.address, To#conn_idx.port, Packet),
            {reply, {ok, {self(), To}}, State}
    end;

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

