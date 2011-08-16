%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP application core.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_core).

-behaviour(gen_server).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip.hrl").

%% API
-export([start_link/0]).
-export([handle_request/2, handle_response/2, lookup_tu/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Types
-record(state, {}).

%% API functions
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% Transport callbacks
-spec handle_request(#sip_connection{}, #sip_message{}) -> ok.
handle_request(_Connection, Msg) when is_record(Msg, sip_message) ->
    % FIXME: reply with 405 Method Not Allowed?
    ok.

-spec handle_response(#sip_connection{}, #sip_message{}) -> ok.
handle_response(_Connection, Msg) when is_record(Msg, sip_message) ->
    ok.

-spec lookup_tu(#sip_connection{}, #sip_message{}) -> {ok, pid()} | undefined.
lookup_tu(_Connection, Msg) when is_record(Msg, sip_message) ->
    % UAS registration is any process with `uas' property registered
    % value of the property is filter function
    Regs = gproc:lookup_local_properties(uas),
    FilteredRegs = lists:dropwhile(fun ({_Pid, Filter}) -> not Filter(Msg) end, Regs),
    case FilteredRegs of
        [{Pid, _} | _Rest] -> {ok, Pid};
        [] -> undefined
    end.

%% Server callbacks

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_call(term(), term(), #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(term(), #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_cast(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
