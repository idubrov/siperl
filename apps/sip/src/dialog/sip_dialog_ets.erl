%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog state tracking using ETS dialogss
%%%
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_dialog_ets).

%% API
-export([start_link/0]).
-export([create_dialog/1, destroy_dialog/1]).
-export([next_local_seq/1, update_remote_seq/2]). % used for constructing/processing requests
-export([lookup_dialog/1, list_dialogs/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {dialogs}).

-type gen_from() :: {pid(), term()}.

%% API

%% @doc Creates dialog server as part of the supervision tree.
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec create_dialog(#sip_dialog{}) -> ok | {error, dialog_exists}.
%% @doc Create new dialog for given pair of request/response
%% @end
create_dialog(Dialog) when is_record(Dialog, sip_dialog) ->
    gen_server:call(?SERVER, {create_dialog, Dialog}).

%% @doc Terminate dialog based on dialog id
%% @end
-spec destroy_dialog(#sip_dialog_id{}) -> {ok, pid()} | {error, no_dialog}.
destroy_dialog(DialogId) when is_record(DialogId, sip_dialog_id) ->
    gen_server:call(?SERVER, {destroy_dialog, DialogId}).

%% @doc Generate new local sequence number and return new state
%% @end
-spec next_local_seq(#sip_dialog_id{}) -> {ok, #sip_dialog{}} | {error, no_dialog}.
next_local_seq(DialogId) when is_record(DialogId, sip_dialog_id) ->
    gen_server:call(?SERVER, {next_local_seq, DialogId}).

%% @doc Update remote sequence number
%% @end
-spec update_remote_seq(#sip_dialog_id{}, sip_sequence()) -> ok | {error, no_dialog} | {error, out_of_order}.
update_remote_seq(DialogId, Sequence) when is_record(DialogId, sip_dialog_id) ->
    gen_server:call(?SERVER, {update_remote_seq, DialogId, Sequence}).

%% @doc Lookup dialog state
%% @end
-spec lookup_dialog(#sip_dialog_id{}) -> {ok, #sip_dialog{}} | {error, no_dialog}.
lookup_dialog(DialogId) when is_record(DialogId, sip_dialog_id) ->
    gen_server:call(?SERVER, {lookup_dialog, DialogId}).

-spec list_dialogs() -> [#sip_dialog{}].
%% @doc List all active dialogs
%% @end
list_dialogs() ->
    gen_server:call(?SERVER, list_dialogs).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    Dialogs = ets:new(?MODULE, [private, set, {keypos, #sip_dialog.id}]),
    {ok, #state{dialogs = Dialogs}}.

%% @private
-spec handle_call({create_dialog, #sip_dialog{}}, gen_from(), #state{}) -> {reply, ok, #state{}};
                 (list_dialogs, gen_from(), #state{}) -> {reply, [#sip_dialog{}], #state{}}.
handle_call({create_dialog, Dialog}, _Client, State) ->
    case ets:insert_new(State#state.dialogs, Dialog) of
        true ->
            {reply, ok, State};
        false ->
            {reply, {error, dialog_exists}, State}
    end;
handle_call({destroy_dialog, DialogId}, _Client, State) ->
    case ets:member(State#state.dialogs, DialogId) of
        true ->
            Owner = ets:lookup_element(State#state.dialogs, DialogId, #sip_dialog.owner),
            true = ets:delete(State#state.dialogs, DialogId),
            {reply, {ok, Owner}, State};
        false ->
            {reply, {error, no_dialog}, State}
    end;
handle_call({next_local_seq, DialogId}, _Client, State) ->
    case catch ets:update_counter(State#state.dialogs, DialogId, {#sip_dialog.local_seq, 1}) of
        {'EXIT', {badarg, _Pos}} ->
            {reply, {error, no_dialog}, State};

        _Sequence ->
            [Dialog] = ets:lookup(State#state.dialogs, DialogId),
            {reply, {ok, Dialog}, State}
    end;
handle_call({update_remote_seq, DialogId, Sequence}, _Client, State) ->
    case catch ets:lookup_element(State#state.dialogs, DialogId, #sip_dialog.remote_seq) of
        {'EXIT', {badarg, _Pos}} ->
            {reply, {error, no_dialog}, State};

        RemoteSequence when RemoteSequence =:= undefined; Sequence > RemoteSequence ->
            true = ets:update_element(State#state.dialogs, DialogId, {#sip_dialog.remote_seq, Sequence}),
            {reply, ok, State};

        % XXX: RFC 3261 says "lower", but we return "out of order" for equal too
        _RemoteSequence ->
            {reply, {error, out_of_order}, State}
    end;
handle_call({lookup_dialog, DialogId}, _Client, State) ->
    case ets:lookup(State#state.dialogs, DialogId) of
        [] ->
            {reply, {error, no_dialog}, State};
        [Dialog] ->
            {reply, {ok, Dialog}, State}
    end;
handle_call(list_dialogs, _Client, State) ->
    List = ets:tab2list(State#state.dialogs),
    {reply, List, State};

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

