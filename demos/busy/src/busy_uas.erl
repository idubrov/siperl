%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(busy_uas).
-extends(sip_uas_callback).

-include_lib("sip/include/sip.hrl").

%% API
-export([init/1, allow/2, 'INVITE'/2, 'CANCEL'/2, handle_info/2]).

-record(context, {timers = dict:new()}).

-spec init({}) -> {ok, #context{}}.
init({}) ->
    {ok, #context{}}.

-spec allow(#sip_request{}, #context{}) -> [atom()].
allow(_Request, _Context) -> ['INVITE', 'CANCEL'].

-spec 'INVITE'(#sip_request{}, #context{}) -> {reply, #sip_response{}, #context{}}.
'INVITE'(Request, Context) ->
    io:format("~s is calling~n", [from(Request)]),

    % Will send busy after 10 seconds
    {ok, Timer} = timer:send_after(10000, {reply, Request}),
    TxKey = sip_transaction:tx_key(server, Request),

    % Send 180 Ringing immediately
    Ringing = sip_message:create_response(Request, 180),

    Timers = dict:store(TxKey, Timer, Context#context.timers),
    {reply, Ringing, Context#context{timers = Timers}}.

-spec 'CANCEL'(#sip_request{}, #context{}) -> {reply, #sip_response{}, #context{}}.
'CANCEL'(Request, Context) ->
    io:format("~s has cancelled the call~n", [from(Request)]),

    % Lookup original transaction and cancel its timer
    TxKey = (sip_transaction:tx_key(server, Request))#sip_tx_server{method = 'INVITE'},
    timer:cancel(dict:fetch(TxKey, Context#context.timers)),
    Timers = dict:erase(TxKey, Context#context.timers),

    % Delegate to standard UAS CANCEL handling
    sip_uas_callback:'CANCEL'(Request, Context#context{timers = Timers}).

-spec handle_info({reply, #sip_request{}}, #context{}) -> {noreply, #context{}}.
handle_info({reply, Request}, Context) ->
    io:format("Telling ~s we are busy~n", [from(Request)]),

    % Send 486 Busy Here response
    Response = sip_uas:create_response(Request, 486),
    sip_uas:send_response(self(), Request, Response),
    {noreply, Context}.

from(Request) ->
    #sip_hdr_address{uri = From} = sip_message:header_top_value(from, Request),
    binary_to_list(sip_uri:format(From)).
