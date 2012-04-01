%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov
-module(busy_uas).
-extends(sip_ua_default).

%% UA callbacks
-export([start_link/0, init/1, is_applicable/1, allow/1, 'INVITE'/2, 'CANCEL'/2, handle_info/2]).

%% Include files
-include_lib("sip/include/sip.hrl").

-define(SERVER, ?MODULE).

-record(context, {requests = dict:new()}).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    sip_ua:start_link({local, ?SERVER}, ?MODULE, {}, []).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------

-spec init({}) -> {ok, #context{}}.
init({}) ->
    [Port | _] = sip_config:ports(udp),
    URI = #sip_uri{scheme = sip, host = binary_to_list(sip_config:self()), port = Port, user = <<"busy">>},
    Contact = sip_headers:address(<<"Busy UAS">>, URI, []),
    ContactStr = binary_to_list(sip_headers:format(contact, Contact)),
    io:format("BUSY: Call me at ~s~n", [ContactStr]),
    {ok, #context{}}.

-spec is_applicable(#sip_request{}) -> boolean().
%% @doc We serve only user `busy', and only requests.
%% Responses will be delivered directly via transaction layer.
%% @end
is_applicable(#sip_request{uri = #sip_uri{user = <<"busy">>}}) -> true;
is_applicable(#sip_request{} = Request) ->
    To = sip_message:header_top_value('to', Request),
    To#sip_hdr_address.uri#sip_uri.user == <<"busy">>.

-spec allow(#sip_request{}) -> [atom()].
allow(_Request) -> ['INVITE', 'CANCEL', 'ACK', 'BYE', 'OPTIONS'].

-spec 'INVITE'(#sip_request{}, #context{}) -> {reply, #sip_response{}, #context{}}.
'INVITE'(Request, Context) ->
    io:format("BUSY: ~s is calling~n", [from(Request)]),

    % Will send busy after 10 seconds
    TimerRef = erlang:send_after(10000, self(), {reply, Request}),
    TxKey = sip_transaction:tx_key(server, Request),

    % Send 180 Ringing immediately
    Ringing = sip_ua:create_response(Request, 180),

    Requests = dict:store(TxKey, {Request, TimerRef}, Context#context.requests),
    {reply, Ringing, Context#context{requests = Requests}}.

-spec 'CANCEL'(#sip_request{}, #context{}) -> {default, #context{}}.
'CANCEL'(Cancel, Context) ->
    % Lookup original transaction and cancel its timer
    TxKey = sip_transaction:tx_key(server, Cancel),
    TxKey2 = TxKey#sip_tx_server{method = 'INVITE'},

    case dict:is_key(TxKey2, Context#context.requests) of
        true ->
            io:format("BUSY: ~s has cancelled the call~n", [from(Cancel)]),

            % cancel timer
            {Invite, TimerRef} = dict:fetch(TxKey2, Context#context.requests),
            _Time = erlang:cancel_timer(TimerRef),

            % reply with 487
            Response = sip_ua:create_response(Invite, 487),
            ok = sip_ua:send_response(Invite, Response),

            % erase request from the dictionary
            Requests = dict:erase(TxKey2, Context#context.requests),

            % reply with 200 to cancel
            Response2 = sip_ua:create_response(Cancel, 200),
            {reply, Response2, Context#context{requests = Requests}};
        false ->
            % reply with 481 to cancel
            Response = sip_ua:create_response(Cancel, 481),
            {reply, Response, Context}
    end.

-spec handle_info({reply, #sip_request{}}, #context{}) -> {noreply, #context{}}.
handle_info({reply, Request}, Context) ->
    io:format("BUSY: Telling ~s we are busy~n", [from(Request)]),

    % Send 486 Busy Here response
    Response = sip_ua:create_response(Request, 486),
    ok = sip_ua:send_response(Request, Response),

    TxKey = sip_transaction:tx_key(server, Request),
    Requests = dict:erase(TxKey, Context#context.requests),
    {noreply, Context#context{requests = Requests}}.

from(Request) ->
    #sip_hdr_address{uri = From} = sip_message:header_top_value(from, Request),
    binary_to_list(sip_uri:format(From)).
