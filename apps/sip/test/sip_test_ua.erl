%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS callback implementation used for testing
%%%
%%% Delegates response generation to configured function.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_ua).
-extends(sip_ua_default).

%% API
-export([start_link/1, send_options/2, send_invite/2]).
-export([detect_loops/2, allow/2, 'OPTIONS'/2, 'INVITE'/2]).
-export([init/1, handle_call/3]).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

-record(state, {handler}).
-type gen_from() :: {pid(), term()}.

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link(fun(() -> ok)) -> {ok, pid()} | {error, term()}.
start_link(Handler) ->
    sip_ua:start_link(?MODULE, {Handler}).

-spec send_options(pid(), #sip_hdr_address{}) -> #sip_response{}.
send_options(Server, To) ->
    Request = sip_ua:create_request('OPTIONS', To),
    gen_server:call(Server, {send_request, Request}).

-spec send_invite(pid(), #sip_hdr_address{}) -> #sip_response{}.
send_invite(Server, To) ->
    Contact = uac_contact(),
    Request = sip_ua:create_request('INVITE', To),
    Request2 = sip_message:append_header(contact, Contact, Request),
    gen_server:call(Server, {send_request, Request2}).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
init({Handler}) ->
    {ok, #state{handler = Handler}}.

detect_loops(_Request, _State) ->
    false.

-spec allow(#sip_request{}, #state{}) -> [atom()].
allow(_Request, _Context) -> ['INVITE', 'OPTIONS'].

'OPTIONS'(Request, #state{handler = Handler} = State) ->
    Response = response(Request, Handler),
    {reply, Response, State}.

'INVITE'(Request, #state{handler = Handler} = State) ->
    Response = response(Request, Handler),
    {reply, Response, State}.


-spec handle_call(term(), gen_from(), #state{}) ->
          {noreply, #state{}} |
          {stop, Reason :: term(), #state{}}.
handle_call({send_request, Request}, From, State) ->
    % Callback ignores provisional responses
    Callback = fun(_Id, {ok, #sip_response{status = Status}}) when Status >= 100, Status =< 199 -> ok;
                  (_Id, Reply) -> gen_server:reply(From, Reply)
               end,
    {ok, _Id} = sip_ua:send_request(Request, Callback),
    {noreply, State};
handle_call(Call, _From, State) ->
    {stop, {unexpected, Call}, State}.

response(Request, Handler) ->
    Response = Handler(Request),
    sip_message:update_top_header(contact, fun(undefined) -> uas_contact(); (Value) -> Value end, Response).

uac_contact() ->
    sip_headers:address(<<>>, <<"sip:test_uac@127.0.0.1">>, []).

uas_contact() ->
    sip_headers:address(<<>>, <<"sip:test_uas@127.0.0.1">>, []).
