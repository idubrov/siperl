%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP transaction layer supervisor
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(SERVER, ?MODULE).

-define(SUPERVISOR(Module, Args),
        {Module, {Module, start_link, Args},
         permanent, infinity, supervisor, [Module]}).

-include("../sip_common.hrl").

%% API functions

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% Supervisor callbacks

%% @private
-spec init({}) -> {ok, _}.
init({}) ->
    Children = [?SUPERVISOR(sip_transaction_tx_sup, [])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

