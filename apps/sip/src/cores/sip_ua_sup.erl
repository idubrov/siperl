%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP UA layer supervisor
%%%
%%% Manages dialog server, dialog event manager
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua_sup).

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
    Children = [?SERVER(sip_dialog_ets, sip_dialog_ets, []),
                ?SERVER(sip_session, sip_session, []),
                ?SERVER(sip_dialog_man, gen_event, [{local, sip_dialog_man}], [dynamic])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

