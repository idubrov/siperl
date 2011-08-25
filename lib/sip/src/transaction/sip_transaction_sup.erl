%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
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
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, _}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------
%% @private
-spec init({}) -> {ok, _}.
init({}) ->
    Children = [?SPEC(sip_transaction_tx_sup, supervisor, [])],
    % Restart the whole layer in case any child fails
    {ok, {{one_for_all, 1000, 3600}, Children}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
