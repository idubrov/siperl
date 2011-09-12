%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pinger UAC implementations. Sends OPTIONS request to the
%%% given destination.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_uac).
-extends(sip_ua).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([start_link/0, request/3, stop/1]).

%% Server callbacks
-export([init/1]).
-export([handle_call/3, handle_response/3, handle_cast/2]).

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
    {ok, #sip_ua_state{callback = ?MODULE}}.

%% @private
handle_call({request, Method, RequestURI}, Client, State) ->
    Request = sip_uac:create_request(Method, RequestURI),
    {ok, State2} = sip_uac:send_request(Request, Client, State),
    {noreply, State2};
handle_call(Req, From, State) ->
    sip_ua:handle_call(Req, From, State).

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast(stop, State) ->
    {stop, normal, State}.

-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> any().
handle_response(Client, Response, State) ->
    Response2 = sip_message:parse_all_headers(Response),
    gen_server:reply(Client, {ok, Response2}),
    pipeline_m:stop({noreply, State}).
