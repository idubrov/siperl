%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Primary application supervisor.
%%%
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("sip_common.hrl").
-include("sip_sup_specs.hrl").

%% API
-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks

%% @private
-spec init(list()) -> {ok, _}.
init([]) ->
    Children = [?SUPERVISOR(sip_transport_sup, []),
                ?SUPERVISOR(sip_transaction_sup, []),
                ?SUPERVISOR(sip_ua_sup, [])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.
