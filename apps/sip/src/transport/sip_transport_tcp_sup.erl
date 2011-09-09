%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% TCP transport implementation supervisor. Starts the TCP transport
%%% support.
%%% ports.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport_tcp_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


-include("../sip_sup_specs.hrl").
-define(SERVER, ?MODULE).

%% API functions
-spec start_link([integer()]) -> {ok, pid()} | ignore | {error, _}.
start_link(Ports) when is_list(Ports) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Ports).

%% Supervisor callbacks

%% @private
-spec init([integer()]) -> {ok, _}.
init(Ports) ->
    Listeners = [?SERVER({listener, Port}, sip_transport_tcp_listener, [Port]) || Port <- Ports], % Listeners
    Children = [?SUPERVISOR(sip_transport_tcp_conn_sup, []) | Listeners],
    {ok, {{one_for_one, 1000, 3600}, Children}}.
