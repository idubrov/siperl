%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc Pinger UAC implementations. Sends OPTIONS request to the
%%% given destination.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_pinger).

%% Exports

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%% API
-export([start_link/0, ping/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

-record(state, {}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    sip_ua:start_link(?MODULE, {}, []).

ping(Pid, To) ->
    gen_server:call(Pid, {ping, To}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

request_failed(Id, _Request, Reason) ->
    ?debugFmt("Request ~p failed due to ~p~n", [Id, Reason]),
    ok.

%% @private
init({}) ->
    ?debugMsg("Initing~n"),
    {ok, #state{}}.

%% @private
handle_info(Req, State) ->
    ?debugFmt("Got info: ~p~n", [Req]),
    {noreply, State}.

%% @private
handle_call({ping, To}, _From, State) ->
    Request = sip_ua:create_request('OPTIONS',
                                    To,
                                    sip_headers:address(<<"Mr. Pinger">>, <<"sip:pinger@127.0.0.1">>, [])),
    {ok, Id} = sip_ua:send_request(Request),
    {reply, Id, State};
handle_call(Req, _From, State) ->
    ?debugFmt("Got call: ~p~n", [Req]),
    {reply, ok, State}.

%% @private
handle_cast(Req, State) ->
    ?debugFmt("Got cast: ~p~n", [Req]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ?debugMsg("Terminating~n"),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

