%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Hang SIP application. Calls other party, then cancels the call after 10 seconds.
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(hang_app).

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
    hang_sup:start_link().

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

