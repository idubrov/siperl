%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP priority set implementation
%%%
%%% Priority set is basically a priority queue that returns added
%%% according to their priority. Every value is returned at most once.
%%%
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-8">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_priority_set).

%% Include files
-include("sip_common.hrl").
-include("sip.hrl").

%% API
-export([new/0, put/3, take/1]).

-type priority_set(T) :: {ordsets:ordset(T), ordsets:ordset({float(), T})}.

-spec new() -> {[], []}.
new() ->
    {ordsets:new(), ordsets:new()}.

-spec put(T, float(), priority_set(T)) -> priority_set(T).
put(Element, Priority, {Added, NotVisited})
  when Priority >= 0.0, Priority =< 1.0 ->
    case ordsets:is_element(Element, Added) of
        true -> {Added, NotVisited}; % Already added to the target set
        false ->
            Added2 = ordsets:add_element(Element, Added),
            InvPri = 1.0 - Priority, % inverted priority
            NotVisited2 = ordsets:add_element({InvPri, Element}, NotVisited),
            {Added2, NotVisited2}
    end.

-spec take(priority_set(T)) -> false | {value, T, priority_set(T)}.
take({_Added, []}) -> false;
take({Added, [{_InvPri, Element} | NotVisited]}) -> {value, Element, {Added, NotVisited}}.


%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec priority_test() -> term().
priority_test() ->
    ?assertEqual(false, take(new())), % Taking from empty

    % Putting twice with different priorities, then taking twice
    Pri = put(hi, 0.5, put(hi, 0.0, new())),
    ?assertMatch({value, hi, _}, take(Pri)),
    ?assertEqual(false, take(element(3, take(Pri)))),

    % Priorities, element with greater priority should come first
    Pri2 = put(hello, 0.5, put(hi, 0.3, new())),
    ?assertMatch({value, hello, _}, take(Pri2)),
    ?assertMatch({value, hi, _}, take(element(3, take(Pri2)))),

    % Same, added in reverse order
    Pri2 = put(hi, 0.3, put(hello, 0.5, new())),
    ?assertMatch({value, hello, _}, take(Pri2)),
    ?assertMatch({value, hi, _}, take(element(3, take(Pri2)))),
    ok.

-endif.

