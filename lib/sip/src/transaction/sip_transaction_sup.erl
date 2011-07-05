%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP transaction layer supervisor.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_sup).

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

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec start_link(sip_config:config()) -> {ok, pid()} | ignore | {error, _}.
start_link(Cfg) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Cfg).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------
%% @private
-spec init(sip_config:config()) -> {ok, _}.
init(Cfg) ->
    Children = [?SPEC(sip_transaction_tx_sup, supervisor, [Cfg]),
                ?WORKER(sip_transaction, [Cfg])],
    % Restart the whole layer in case any child fails
    {ok, {{one_for_all, 1000, 3600}, Children}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
