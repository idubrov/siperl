%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% This supervisor spawns processes that handles TCP connections.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_tcp_conn_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/0, start_connection/1]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transport.hrl").

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Start new connection. Socket is opened by the connection process.
%% @end
-spec start_connection(#sip_endpoint{} | inet:socket()) -> {ok, pid()} | {error, any()}.
start_connection(RemoteEndpoint)
  when is_record(RemoteEndpoint, sip_endpoint) ->
    supervisor:start_child(?SERVER, [RemoteEndpoint]);

%% @doc
%% Start worker process and transfer socket ownership to it.
%% @end
start_connection(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init([]) -> {ok, _}.
init([]) ->
    % TCP connection child spec
    Module = sip_transport_tcp_conn,
    Worker = {Module, {Module, start_link, []}, temporary, 2000, worker, [Module]},
    {ok, {{simple_one_for_one, 1000, 3600}, [Worker]}}.

