%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% This supervisor spawns processes that handle transactions.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transaction_tx_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/0, start_tx/2, start_fsm_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip_transaction.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @doc
%% Start transaction FSM process.
%% @end
-spec start_tx(module(), sip_tx_key()) -> supervisor:start_child_ret().
start_tx(Module, TxKey) ->
    supervisor:start_child(?SERVER, [Module, TxKey]).

%% @private
%% Start FSM implemented by given module.
-spec start_fsm_link(module(), sip_tx_key()) ->
          {ok, pid()} | ignore | {error, term()}.
start_fsm_link(Module, TxKey) ->
    gen_fsm:start_link(Module, TxKey, []).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, term()}.
init({}) ->
    Worker = {tx, {sip_transaction_tx_sup, start_fsm_link, []}, temporary, 2000, worker, dynamic},
    {ok, {{simple_one_for_one, 1000, 3600}, [Worker]}}.

