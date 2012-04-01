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
-export(['OPTIONS'/2, 'INVITE'/2, 'CANCEL'/2]).
-export([init/1, handle_cast/2]).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

-record(state, {handler,
                requests = [] :: [{#sip_tx_server{}, #sip_request{}}]}).

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

'OPTIONS'(Request, State) ->
    handle_request(Request, State).

'INVITE'(Request, State) ->
    handle_request(Request, State).

'CANCEL'(Cancel, State) ->
    TxKey = sip_transaction:tx_key(server, Cancel),
    TxKey2 = TxKey#sip_tx_server{method = 'INVITE'}, % only 'INVITE's are cancellable right now
    case lists:keyfind(TxKey2, 1, State#state.requests) of
        false ->
            do_send_response(Cancel, sip_ua:create_response(Cancel, 481)),
            % no transaction... 481
            {noreply, State};
        {TxKey2, Invite} ->
            do_send_response(Invite, sip_ua:create_response(Invite, 487)),
            do_send_response(Cancel, sip_ua:create_response(Cancel, 200)),

            % stop tracking the request
            Requests = lists:keydelete(TxKey2, 1, State#state.requests),
            {noreply, State#state{requests = Requests}}
    end.

handle_cast({reply, TxKey, Response}, State) ->
    case lists:keyfind(TxKey, 1, State#state.requests) of
        false ->
            % was canceled or already replied
            {noreply, State};
        {TxKey, Request} ->
            % add Contact: header and send response
            do_send_response(Request, Response),

            % stop tracking the request
            Requests = lists:keydelete(TxKey, 1, State#state.requests),
            {noreply, State#state{requests = Requests}}
    end;
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

handle_request(Request, #state{handler = Handler, requests = Requests} = State) ->
    Self = self(),
    TxKey = sip_transaction:tx_key(server, Request),
    ReplyFun =
        fun (Response) ->
            gen_server:cast(Self, {reply, TxKey, Response})
        end,
    Handler(Request, ReplyFun),
    {noreply, State#state{requests = [{TxKey, Request} | Requests]}}.

contact() ->
    sip_headers:address(<<>>, <<"sip:uas@127.0.0.1">>, []).

do_send_response(Request, Response) ->
    UpdateFun = fun(undefined) -> contact(); (Value) -> Value end,
    Response2 = sip_message:update_top_header(contact, UpdateFun, Response),
    sip_ua:send_response(Request, Response2).
