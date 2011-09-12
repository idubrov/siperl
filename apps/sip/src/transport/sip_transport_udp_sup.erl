%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% UDP transport implementation supervisor. Starts UDP transport
%%% support.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport_udp_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("../sip_sup_specs.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link([integer()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ports) when is_list(Ports) ->
    supervisor:start_link(?MODULE, Ports).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init([integer()]) -> {ok, _}.
init(Ports) ->
    Listeners = [?SERVER({socket, Port}, sip_transport_udp_socket, [Port]) || Port <- Ports],
    Children = [?SUPERVISOR(sip_transport_udp_socket_sup, []) | Listeners],
    {ok, {{one_for_one, 1000, 3600}, Children}}.
