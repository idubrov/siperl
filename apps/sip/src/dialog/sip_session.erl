%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP session state server
%%%
%%% State server that tracks state of sessions. Session state could be updated
%%% only by a valid offer/response pair.
%%%
%%% Session could be in one of the following states:
%%% <ul>
%%% <li>Offered (offer received). Local and remote session descriptors are
%%% empty (undefined). When offer is answered, local and remote session
%%% descriptors are updated at once.
%%% <li>Answered (valid answer received). Local and remote session descriptors
%%% are present.
%%% <li>Re-offered (offer received after session was in answered state). Local
%%% and remote session descriptors contain previous values, new offer is pending.
%%% When offer is answered, local and remote session descriptors are updated at
%%% once.
%%% </ul>
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_session).

%% API
-export([start_link/0, offer/3, answer/3, lookup/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {table}).

-type gen_from() :: {pid(), term()}.

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
%% @doc Creates session server as part of the supervision tree.
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec offer(sip_session_id(), local | remote, #sip_session_desc{}) -> ok | {error, Reason :: term()}.
%% @doc Make an offer to the session identified by given id
%% @end
offer(SessionId, Side, Offer) when Side =:= local; Side =:= remote ->
    gen_server:call(?SERVER, {offer, SessionId, Side, Offer}).

-spec answer(sip_session_id(), local | remote, #sip_session_desc{}) -> ok | {error, Reason :: term()}.
%% @doc Apply an answer to the session identified by given id
%% @end
answer(SessionId, Side, Answer) when Side =:= local; Side =:= remote ->
    gen_server:call(?SERVER, {answer, SessionId, Side, Answer}).

-spec lookup(sip_session_id()) -> {ok, #sip_session{}} | {error, Reason :: term()}.
lookup(SessionId) ->
    gen_server:call(?SERVER, {lookup, SessionId}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    Table = ets:new(?MODULE, [private, set, {keypos, #sip_session.id}]),
    {ok, #state{table = Table}}.

%% @private
-spec handle_call
        ({offer, #sip_dialog_id{}, local | remote, #sip_session_desc{}}, gen_from(), #state{}) ->
          {reply, ok, #state{}} |
          {reply, {error, already_offered}, #state{}};
        ({answer, #sip_dialog_id{}, local | remote, #sip_session_desc{}}, gen_from(), #state{}) ->
          {reply, ok, #state{}} |
          {reply, {error, no_session}, #state{}} |
          {reply, {error, not_offered}, #state{}} |
          {reply, {error, wrong_side}, #state{}};
        ({lookup, #sip_dialog_id{}}, gen_from(), #state{}) ->
          {reply, {ok, #sip_session{}}, #state{}} |
          {reply, {error, no_session}, #state{}}.
handle_call({offer, SessionId, Side, SessionDesc}, _From, State) ->
    case lookup_create(SessionId, State) of
        #sip_session{offer = undefined} = Session ->
            Offer = #sip_offer{side = Side, session = SessionDesc},
            Session2 = Session#sip_session{offer = Offer},
            true = ets:insert(State#state.table, Session2),
            {reply, ok, State};

        #sip_session{} ->
            {reply, {error, already_offered}, State}
    end;

handle_call({answer, SessionId, Side, Answer}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [] ->
            {reply, {error, no_session}, State};
        [#sip_session{offer = undefined}] ->
            {reply, {error, not_offered}, State};
        [#sip_session{offer = #sip_offer{side = Side}}] ->
            {reply, {error, wrong_side}, State};
        [#sip_session{} = Session] when Answer =:= none ->
            % cancel offer
            Session2 = Session#sip_session{offer = undefined},
            true = ets:insert(State#state.table, Session2),
            {reply, ok, State};
        [#sip_session{offer = #sip_offer{session = Offer}} = Session] ->
            % update session descriptors
            {Local, Remote} =
                if Side =:= local -> {Answer, Offer};
                   Side =:= remote -> {Offer, Answer}
                end,
            Session2 = Session#sip_session{offer = undefined, local = Local, remote = Remote},
            true = ets:insert(State#state.table, Session2),
            {reply, ok, State}
    end;

handle_call({lookup, SessionId}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [] ->
            {reply, {error, no_session}, State};
        [#sip_session{} = Session] ->
            {reply, {ok, Session}, State}
    end;

handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {stop, {unexpected, term()}, #state{}}.
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(term(), #state{}) -> {stop, {unexpected, term()}, #state{}}.
handle_info(Info, State) ->
    {stop, {unexpected, Info}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
lookup_create(SessionId, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [] -> #sip_session{id = SessionId};
        [#sip_session{} = Session] -> Session
    end.
