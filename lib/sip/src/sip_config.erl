%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Configuration module.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_config).

%% Exports
-export([t1/0, t2/0, t4/0, ports/1, self/0, configured_routes/0]).

%% Includes
-include_lib("sip_common.hrl").

%% API functions

%% @doc
%% Get T1 timer value. See RFC 3261 Appendix A.
%% @end
-spec t1() -> integer().
t1() ->
    entry(t1, 500).

%% @doc
%% Get T2 timer value. See RFC 3261 Appendix A.
%% @end
-spec t2() -> integer().
t2() ->
    entry(t2, 4000).

%% @doc
%% Get T4 timer value. See RFC 3261 Appendix A.
%% @end
-spec t4() -> integer().
t4() ->
    entry(t4, 5000).

%% @doc
%% Get ports for given transport.
%% @end
-spec ports(atom()) -> [integer()].
ports(udp) -> entry(udp, [5060]);
ports(tcp) -> entry(tcp, [5060]).

-spec self() -> binary().
self() ->
    {ok, Hostname} = inet:gethostname(),
    entry(self, list_to_binary(Hostname)).

-spec configured_routes() -> [binary()].
configured_routes() ->
    [].

%%%----------------------------------------------------------------
%% Internal functions
%%%----------------------------------------------------------------
entry(Key, Default) when is_atom(Key) ->
    case application:get_env(Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.
