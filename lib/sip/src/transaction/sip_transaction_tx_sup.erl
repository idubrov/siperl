%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% This supervisor spawns processes that handle transactions.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_tx_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/1, start_tx/3, start_fsm_link/4]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_message.hrl").

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(T1) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, T1).

%% @doc
%% Start transaction FSM process.
%% @end
-spec start_tx({sip_transaction:tx_key(), module()}, term(), {To :: #sip_endpoint{}, Msg :: #sip_message{}}) ->
		  supervisor:start_child_ret().
start_tx(KeyModule, TxUser, ToMsg) ->
	supervisor:start_child(?SERVER, [KeyModule, TxUser, ToMsg]).

%% @private
%% Start FSM implemented by given module.
-spec start_fsm_link(sip_config:config(), 
					 {sip_transaction:tx_key(), module()}, 
					 term(), 
					 {To :: #sip_endpoint{}, Msg :: #sip_message{}}) ->
		  {ok, pid()} | ignore | {error, term()}.
start_fsm_link(Cfg, {Key, Module}, TxUser, {To, Msg}) ->
	gen_fsm:start_link(Module, {Cfg, Key, TxUser, {To, Msg}}, []).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(sip_config:config()) -> {ok, term()}.
init(Cfg) ->
	Worker = {tx, {sip_transaction_tx_sup, start_fsm_link, [Cfg]}, temporary, 2000, worker, dynamic},
    {ok, {{simple_one_for_one, 1000, 3600}, [Worker]}}.

