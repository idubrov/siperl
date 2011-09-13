%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pinger UAC implementations. Sends OPTIONS request to the
%%% given destination.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_uac).
-behaviour(gen_server).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([start_link/0, request/3, stop/1, handle_response/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

-record(state, {uac}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

request(Pid, Method, RequestURI) ->
    gen_server:call(Pid, {request, Method, RequestURI}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    {ok, #state{uac = sip_uac:new([])}}.

%% @private
handle_call({request, Method, RequestURI}, Client, #state{uac = UAC} = State) ->
    Request = sip_uac:create_request(UAC, Method, RequestURI),
    ok = sip_uac:send_request(UAC, Request, Client),
    {noreply, State}.

%% @private
-spec handle_cast(_, #state{}) -> any().
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info({response, Response, ReqInfo}, State) ->
    UAC = State#state.uac,
    case sip_uac:process_response(UAC, Response, ReqInfo) of
        {ok, Client} ->
            handle_response(Response, Client),
            {noreply, State};
        {error, _Reason} ->
            {noreply, State}
    end;
handle_info({tx, _TxKey, {terminated, _Reason}}, State) ->
    {noreply, State}.

-spec handle_response(#sip_response{}, term()) -> ok.
handle_response(Response, Client) ->
    Response2 = sip_message:parse_all_headers(Response),
    gen_server:reply(Client, {ok, Response2}),
    ok.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

