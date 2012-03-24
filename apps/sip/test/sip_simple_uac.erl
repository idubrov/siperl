%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC module used for testing
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov
-module(sip_simple_uac).
-extends(sip_ua_default).

%% API
-export([start_link/0, send_options/2, send_invite/2, send_request/2, cancel/1]).
-export([init/1, is_applicable/1, handle_call/3, handle_response/4]).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

-record(state, {requests = [] :: [{reference(), #sip_request{}}]}).   % outgoing requests
-type gen_from() :: {pid(), term()}.


%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    sip_ua:start_link(?MODULE, {}, []).

-spec send_options(pid(), #sip_hdr_address{}) -> {ok, #sip_response{}}.
send_options(Server, To) ->
    Request = sip_ua:create_request('OPTIONS', To),
    gen_server:call(Server, {send_request, Request}).

-spec send_invite(pid(), #sip_hdr_address{}) -> {ok, #sip_response{}}.
send_invite(Server, To) ->
    Request = create_invite(To),
    gen_server:call(Server, {send_request, Request}).

-spec send_request(pid(), #sip_request{}) -> {ok, #sip_response{}}.
send_request(Server, Request) ->
    gen_server:call(Server, {send_request, Request}).

%% @doc Cancel last request sent (even if it was answered already)
%% @end
-spec cancel(pid()) -> ok.
cancel(Server) ->
    gen_server:call(Server, cancel).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
init({}) ->
    {ok, #state{}}.

%% @doc UAC does not handle any requests
%% @end
-spec is_applicable(#sip_request{}) -> false.
is_applicable(_Request) -> false.

-spec handle_call(term(), gen_from(), #state{}) ->
          {noreply, #state{}} |
          {stop, Reason :: term(), #state{}}.
handle_call({send_request, Request}, From, State) ->
    {ok, Id} = sip_ua:send_request(Request),
    Requests = [{Id, From} | State#state.requests],
    {noreply, State#state{requests = Requests}};
handle_call(cancel, _From, State) ->
    [{RequestId, _From2} | _Rest] = State#state.requests,
    ok = sip_ua:cancel_request(RequestId),
    {reply, ok, State};
handle_call(Call, _From, State) ->
    {stop, {unexpected, Call}, State}.

handle_response(_Request, #sip_response{status = Status}, _RequestId, State) when Status >= 100, Status =< 199 ->
    % Ignore provisional responses
    {noreply, State};
handle_response(_Request, Response, RequestId, State) ->
    {RequestId, From} = lists:keyfind(RequestId, 1, State#state.requests),
    gen_server:reply(From, {ok, Response}),
    % note: always leaving last request id on the top, so we can use it for cancel, for example
    {noreply, State}.

contact() ->
    sip_headers:address(<<>>, <<"sip:uac@127.0.0.1">>, []).

create_invite(To) ->
    Contact = contact(),
    Request = sip_ua:create_request('INVITE', To),
    Request2 = sip_message:append_header(contact, Contact, Request),
    sip_message:append_header('content-type', <<"application/sdp">>, Request2).
