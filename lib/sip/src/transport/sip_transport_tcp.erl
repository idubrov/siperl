%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% TCP transport implementation module.
%%% ports.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_tcp).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Transport callbacks
-export([send/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

%% Records
-record(state, {ports}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link([integer()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ports) when is_list(Ports) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Ports, []).

%%-----------------------------------------------------------------
%% Transport callbacks
%%-----------------------------------------------------------------
-spec send(#sip_destination{} | pid(), #sip_message{}) -> {ok, pid()} | {error, Reason :: term()}.
send(To, Message) when is_record(To, sip_destination) ->
    {ok, Pid} = sip_transport_tcp_conn_sup:start_connection(To),
    send(Pid, Message);
send(Pid, Message) when is_pid(Pid), is_record(Message, sip_message) ->
    sip_transport_tcp_conn:send(Pid, Message).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init([integer()]) -> {ok, #state{}}.
init(Ports) ->
    {ok, #state{ports = Ports}}.

%% @private
-spec handle_call(_, _, #state{}) -> {stop, {unexpected, _}, #state{}}.
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