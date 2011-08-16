%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc Pingable UAS implementations. Replies to the OPTIONS
%%% requests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_pingable).

-behaviour(sip_ua).

%% Exports

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%% API
-export([start_link/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2, handle_request/3, handle_response/3]).

-record(state, {}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    sip_ua:start_link(?MODULE, {}, []).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    gproc:add_local_property(uas, fun is_applicable/1),
    {ok, #state{}}.

%% @private
handle_response(_Response, _UserData, State) ->
    {noreply, State}.

%% @private
handle_request(Request, _UserData, State) ->
    ?debugHere,
    Response = sip_message:create_response(Request, 200, <<"Ok. Hello!">>),
    sip_ua:send_response(Response),
    ?debugHere,
    {noreply, State}.

%% @private
handle_info(Req, State) ->
    {noreply, State}.

%% @private
handle_call(Req, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(Req, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_applicable(#sip_message{kind = #sip_request{method = 'OPTIONS'}}) -> true;
is_applicable(_Msg) -> false.
