%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Busy SIP application. Starts UAS to respond 486 Busy Here for all INVITE's
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(busy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%-----------------------------------------------------------------
%% Application callbacks
%%-----------------------------------------------------------------

%% @private
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid()} |
                      {error, Reason::any()}.
start(_StartType, _StartArgs) ->
    busy_sup:start_link().

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

