%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pingable UAS implementations. Replies to the OPTIONS
%%% requests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_uas).
-behaviour(gen_server).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([start_link/0, set_handler/2, stop/1, handle_request/3]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

-record(state, {uas, handler}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

set_handler(Pid, Fun) ->
    gen_server:call(Pid, {set_handler, Fun}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    sip_cores:register_core(#sip_core_info{is_applicable = fun is_applicable/1}),
    UAS = sip_uas:new([{allow, ['OPTIONS']}, no_detect_loops]),
    {ok, #state{uas = UAS}}.

%% @private
handle_call({set_handler, Handler}, _Client, #state{} = State) ->
    {reply, ok, State#state{handler = Handler}}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info({request, Msg}, State) ->
    case sip_uas:process_request(State#state.uas, Msg) of
        ok ->
            handle_request(sip_message:method(Msg), Msg, State),
            {noreply, State};
        {error, _Reason} ->
            {noreply, State}
    end;
handle_info({tx, _TxKey, {terminated, _Reason}}, State) ->
    {noreply, State}.

%% @private
handle_request(_Method, Request, #state{handler = Handler, uas = UAS}) ->
    Response = Handler(Request),
    sip_uas:send_response(UAS, Response),
    ok.

%% @private
-spec handle_cast(_, #state{}) -> any().
handle_cast(stop, State) ->
    {stop, normal, State}.


%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



is_applicable(_Msg) -> true.
