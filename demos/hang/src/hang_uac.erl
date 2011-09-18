%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(hang_uac).
-behaviour(gen_server).

%% API
-export([start_link/0, call/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

%% Include files
-include_lib("sip/include/sip.hrl").

-record(state, {uac :: pid(),
                timers = dict:new()}).

-type gen_from() :: {pid(), term()}.

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec call(string()) -> ok.
call(To) ->
    URI = sip_uri:parse(list_to_binary(To)),
    gen_server:call(?SERVER, {call, URI}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, UAC} = sip_uac:start_link(),
    {ok, #state{uac = UAC}}.

%% @private
-spec handle_call({call, #sip_uri{}}, gen_from(), #state{}) -> {reply, #sip_request{}, #state{}}.
handle_call({call, To}, _From, #state{uac = UAC} = State) ->
    Request = sip_uac:create_request(UAC, 'INVITE', sip_headers:address(<<>>, To, [])),
    Contact = sip_headers:address(<<"Hang">>, <<"sip:127.0.0.1:5060">>, []),
    Request2 = sip_message:append_header(contact, Contact, Request),

    % Call
    io:format("Calling to ~s~n", [to(Request)]),
    {ok, Ref} = sip_uac:send_request(UAC, Request2),

    % Arm timer to cancel call after 2 seconds
    {ok, Timer} = timer:send_after(2000, {cancel, Ref}),

    % Store timer
    Timers = dict:store(Ref, Timer, State#state.timers),
    {reply, ok, State#state{timers = Timers}};
handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({response, Ref, #sip_response{status = Status, reason = Reason} = Response}, State) ->
    Timer = dict:fetch(Ref, State#state.timers),
    case Status of
        _ when Status >= 100, Status =< 199 ->
            io:format("Progress from ~s: ~w ~s~n", [to(Response), Status, binary_to_list(Reason)]),
            {noreply, State};
        _Other ->
            timer:cancel(Timer), % cancel timer in any case of final response

            io:format("Got final response ~s: ~w ~s~n", [to(Response), Status, binary_to_list(Reason)]),
            Timers = dict:erase(Ref, State#state.timers),
            {noreply, State#state{timers = Timers}}
    end;
handle_info({cancel, Id}, State) ->
    % Cancel request
    io:format("Hanging up~n"),
    ok = sip_uac:cancel(State#state.uac, Id),
    {noreply, State};
handle_info({response, #sip_response{status = Status, reason = Reason}}, State) ->
    io:format("Got response for CANCEL ~w ~s~n", [Status, binary_to_list(Reason)]),
    {noreply, State};
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

to(Msg) ->
    #sip_hdr_address{uri = To} = sip_message:header_top_value(to, Msg),
    binary_to_list(sip_uri:format(To)).
