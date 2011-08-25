%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% TCP transport implementation supervisor. Starts the TCP transport
%%% support.
%%% ports.
%%% @end
%%% @copyright 2011 Ivan Dubrov
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

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(LISTENER(Port),
        {{listener, Port}, {sip_transport_tcp_listener, start_link, [Port]},
             permanent, 2000, worker, [sip_transport_tcp_listener]}).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link([integer()]) -> {ok, pid()} | ignore | {error, _}.
start_link(Ports) when is_list(Ports) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Ports).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init([integer()]) -> {ok, _}.
init(Ports) ->
    Children = [?SPEC(sip_transport_tcp_conn_sup, supervisor) | % Supervisor for connections
                [?LISTENER(Port) || Port <- Ports] % Listeners
               ],
    {ok, {{one_for_one, 1000, 3600}, Children}}.
