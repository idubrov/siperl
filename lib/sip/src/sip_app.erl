%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP application. Starts SIP listeners on configured endpoints. 
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_app).

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
    case sip_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

