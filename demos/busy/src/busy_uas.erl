%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(busy_uas).
-extends(sip_uas_callback).

-include_lib("sip/include/sip.hrl").

%% API
-export([init/1, allow/2, 'INVITE'/2, handle_info/2]).

-record(context, {}).

-spec init({}) -> {ok, #context{}}.
init({}) ->
    {ok, #context{}}.

-spec allow(#sip_request{}, #context{}) -> [atom()].
allow(_Request, _Context) -> ['INVITE'].

-spec 'INVITE'(#sip_request{}, #context{}) -> {reply, #sip_response{}, #context{}}.
'INVITE'(Request, Context) ->
    timer:send_after(10000, {reply, Request}),

    % Send 180 Ringing immediately
    Ringing = sip_message:create_response(Request, 180),
    {reply, Ringing, Context}.

-spec handle_info({reply, #sip_request{}}, #context{}) -> {reply, #sip_request{}, #sip_response{}, #context{}}.
handle_info({reply, Request}, Context) ->
    % Send 486 Busy Here response
    Response = sip_uas:create_response(Request, 486),
    {reply, Request, Response, Context}.
