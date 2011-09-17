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

-record(state, {uac, calls}).

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
    {ok, #state{uac = UAC, calls = dict:new()}}.

%% @private
-spec handle_call({call, #sip_uri{}}, gen_from(), #state{}) -> {reply, #sip_request{}, #state{}}.
handle_call({call, To}, _From, #state{uac = UAC} = State) ->
    Request = sip_uac:create_request(UAC, 'INVITE', sip_headers:address(<<>>, To, [])),
    Contact = sip_headers:address(<<"Hang">>, <<"sip:127.0.0.1:5060">>, []),
    Request2 = sip_message:append_header(contact, Contact, Request),

    % Call
    io:format("Calling to ~s~n", [to(Request)]),
    Self = self(),
    Id = sip_idgen:generate_id(8),
    ok = sip_uac:send_request(UAC, Request2, fun({ok, Response}) -> Self ! {response, Id, Response} end),

    % Arm timer to cancel call after 2 seconds
    {ok, Timer} = timer:send_after(2000, {cancel, Id}),

    % Store our request for later use
    Calls = dict:store(Id, {Timer, Request}, State#state.calls),
    {reply, ok, State#state{calls = Calls}};
handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({response, Id, #sip_response{status = Status, reason = Reason}}, State) ->
    {Timer, Request} = dict:fetch(Id, State#state.calls),
    case Status of
        _ when Status >= 100, Status =< 199 ->
            io:format("Progress from ~s: ~w ~s~n", [to(Request), Status, binary_to_list(Reason)]),
            {noreply, State};
        _Other ->
            timer:cancel(Timer),

            io:format("Got final response ~s: ~w ~s~n", [to(Request), Status, binary_to_list(Reason)]),
            Calls = dict:erase(Id, State#state.calls),
            {noreply, State#state{calls = Calls}}
    end;
handle_info({cancel, Id}, State) ->
    {_Timer, Request} = dict:fetch(Id, State#state.calls),

    % Cancel transaction
    io:format("Hanging up to ~s~n", [to(Request)]),
    {ok, _TxKey} = sip_transaction:cancel(Request),
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

to(Request) ->
    #sip_hdr_address{uri = To} = sip_message:header_top_value(to, Request),
    binary_to_list(sip_uri:format(To)).
