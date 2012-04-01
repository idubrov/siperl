%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Sessions manager for hang UA
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov
-module(hang_ua_sessions).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% Include files
-include_lib("sip/include/sip.hrl").

-record(state, {ua :: pid(), timers = []}).

-spec init({pid()}) -> {ok, #state{}}.
init({UA}) ->
    {ok, #state{ua = UA}}.

%% FIXME: How should we filter dialogs? We are not interested in all of them...
-spec handle_event(any(), #state{}) -> {ok, #state{}}.
handle_event({dialog_created, DialogId, Owner}, State) when State#state.ua =:= Owner ->
    % Hang up after 5 seconds
    TimerRef = erlang:send_after(5000, self(), {bye, DialogId}),
    Timers = [{DialogId, TimerRef} | State#state.timers],
    {ok, State#state{timers = Timers}};
handle_event({dialog_terminated, DialogId, _Owner}, State) ->
    State2 = cancel_bye_timer(DialogId, State),
    {ok, State2};
handle_event(_Event, State) ->
    {ok, State}.

-spec handle_call(any(), #state{}) -> {ok, ok, #state{}}.
handle_call(_Request, State) ->
    {ok, ok, State}.

-spec handle_info({bye, #sip_dialog_id{}}, #state{}) -> {ok, #state{}}.
handle_info({bye, DialogId}, State) ->
    % handle timer message, send BYE request
    io:format("HANG: Sending BYE~n"),
    hang_ua:bye(DialogId),
    {ok, State}.

-spec terminate(any(), #state{}) -> ok.
terminate(_Arg, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cancel_bye_timer(DialogId, State) ->
    case lists:keyfind(DialogId, 1, State#state.timers) of
        false -> State;
        {DialogId, TimerRef} ->
            _Ignore = erlang:cancel_timer(TimerRef),
            receive {bye, _To, DialogId} -> ok % flush timer message
            after 0 -> ok
            end,
            Timers = lists:keydelete(DialogId, 1, State#state.timers),
            State#state{timers = Timers}
    end.