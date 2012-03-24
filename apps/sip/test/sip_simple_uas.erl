%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS callback implementation used for testing
%%%
%%% Delegates response generation to configured function.
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov
-module(sip_simple_uas).
-extends(sip_ua_default).

%% API
-export([start_link/1]).
-export(['OPTIONS'/2, 'INVITE'/2]).
-export([init/1, handle_cast/2]).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

-record(state, {handler}).

-type handler() :: fun((#sip_request{}, fun((#sip_response{}) -> ok)) -> ok).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link(handler()) -> {ok, pid()} | {error, term()}.
start_link(Handler) ->
    sip_ua:start_link(?MODULE, {Handler}, [no_detect_loops]).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
init({Handler}) ->
    {ok, #state{handler = Handler}}.

'OPTIONS'(Request, #state{handler = Handler} = State) ->
    response(Request, Handler),
    {noreply, State}.

'INVITE'(Request, #state{handler = Handler} = State) ->
    response(Request, Handler),
    {noreply, State}.

handle_cast({reply, Request, Response}, State) ->
    UpdateFun = fun(undefined) -> contact(); (Value) -> Value end,
    Response2 = sip_message:update_top_header(contact, UpdateFun, Response),
    sip_ua:send_response(Request, Response2),
    {noreply, State};
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

response(Request, Handler) ->
    Self = self(),
    ReplyFun = fun (Response) -> gen_server:cast(Self, {reply, Request, Response}) end,
    % pass the reply fun as a last parameter
    Handler(Request, ReplyFun),
    ok.

contact() ->
    sip_headers:address(<<>>, <<"sip:uas@127.0.0.1">>, []).
