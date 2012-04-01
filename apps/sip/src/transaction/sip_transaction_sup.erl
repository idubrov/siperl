%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP transaction layer supervisor
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-include("../sip_common.hrl").
-include("../sip_sup_specs.hrl").

%% API functions

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
start_link() ->
    supervisor:start_link(?MODULE, {}).

%% Supervisor callbacks

%% @private
-spec init({}) -> {ok, _}.
init({}) ->
    Children = [?SUPERVISOR(sip_transaction_tx_sup, [])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

