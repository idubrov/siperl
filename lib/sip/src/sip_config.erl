%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Configuration module.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_config).

%% Exports
-export([t1/1, t2/1, t4/1, ports/2, router/1, app/1, self/1, from_options/1]).

%% Includes
-include_lib("sip_common.hrl").
-include_lib("sip_message.hrl").

%% Types
-opaque config() :: term().
-export_type([config/0]).

%% API functions

%% @doc
%% Get T1 timer value. See RFC 3261 Appendix A.
%% @end
-spec t1(config()) -> integer().
t1(Cfg) ->
    entry(Cfg, t1, 500).

%% @doc
%% Get T2 timer value. See RFC 3261 Appendix A.
%% @end
-spec t2(config()) -> integer().
t2(Cfg) ->
    entry(Cfg, t2, 4000).

%% @doc
%% Get T4 timer value. See RFC 3261 Appendix A.
%% @end
-spec t4(config()) -> integer().
t4(Cfg) ->
    entry(Cfg, t4, 5000).

%% @doc
%% Get ports for given transport.
%% @end
-spec ports(config(), atom()) -> [integer()].
ports(Cfg, udp) -> entry(Cfg, udp, [5060]);
ports(Cfg, tcp) -> entry(Cfg, tcp, [5060]).

-spec router(config()) -> atom().
router(Cfg) ->
    entry(Cfg, router, no_router).

-spec app(config()) -> atom().
app(Cfg) ->
    entry(Cfg, app, no_app).

-spec self(config()) -> binary().
self(Cfg) ->
    {ok, Hostname} = inet:gethostname(),
    entry(Cfg, self, list_to_binary(Hostname)).

%% @doc
%% Create configuration from list of options.
%% @end
-spec from_options([{Name :: atom(), Value :: term()}]) -> config().
from_options(List) ->
    List.

%%%----------------------------------------------------------------
%% Internal functions
%%%----------------------------------------------------------------
entry(Cfg, Param, Default) when is_list(Cfg), is_atom(Param) ->
    case lists:keyfind(Param, 1, Cfg) of
        {Param, Value} -> Value;
        false -> Default
    end.
