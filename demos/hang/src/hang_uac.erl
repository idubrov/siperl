%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(hang_uac).
-extends(sip_ua_default).

%% API
-export([start_link/0, call/1, init/1]).

%% UA callbacks
-export([handle_info/2]).

%% Include files
-include_lib("sip/include/sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {timers = dict:new()}).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    sip_ua:start_link({local, ?SERVER}, ?MODULE, {}).

-spec call(string()) -> ok.
call(To) ->
    URI = sip_uri:parse(list_to_binary(To)),
    ?SERVER ! {call, URI}.

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({call, To}, #state{} = State) ->
    Request = sip_ua:create_request('INVITE', sip_headers:address(<<>>, To, [])),
    Contact = sip_headers:address(<<"Hang">>, <<"sip:127.0.0.1:5060">>, []),
    Request2 = sip_message:append_header(contact, Contact, Request),

    % Call
    io:format("Calling to ~s~n", [to(Request)]),
    {ok, Ref} = sip_ua:send_request(Request2),

    % Arm timer to cancel call after 2 seconds
    {ok, Timer} = timer:send_after(2000, {cancel, Ref}),

    % Store timer
    Timers = dict:store(Ref, Timer, State#state.timers),
    {noreply, State#state{timers = Timers}};

handle_info({response, #sip_response{status = Status, reason = Reason} = Response, Ref}, State) ->
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
    Result = sip_ua:cancel_request(Id),
    io:format("Hanging up: ~p~n", [Result]),
    {noreply, State};
handle_info(Info, State) ->
    {stop, {unexpected, Info}, State}.

to(Msg) ->
    #sip_hdr_address{uri = To} = sip_message:header_top_value(to, Msg),
    binary_to_list(sip_uri:format(To)).
