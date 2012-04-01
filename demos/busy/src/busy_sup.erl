%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Primary application supervisor.
%%%
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(busy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% API
-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link(?SERVER, []).

%% Supervisor callbacks

%% @private
-spec init(list()) -> {ok, _}.
init([]) ->
    Children = [{busy_uas, {busy_uas, start_link, []},
                 permanent, 2000, worker, [busy_uas]}],
    {ok, {{one_for_one, 1000, 3600}, Children}}.
