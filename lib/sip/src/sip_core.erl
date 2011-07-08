%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP application core.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_core).

-behaviour(gen_server).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip_message.hrl").

%% API
-export([start_link/0]).
-export([handle/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Types
-record(state, {}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec handle(sip_transport:connection(), #sip_message{}) -> ok.
handle(Connection, Msg)
  when is_record(Msg, sip_message) ->
    % FIXME: core layer implementation..
    % XXX: Start new transaction for requests...
    case sip_message:is_request(Msg) of
        true ->
            sip_transaction:start_tx(server, whereis(sip_core), Connection, Msg),
            ok;
        _ ->
            ok
    end.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_call(_, _, #state{}) ->
          {stop, {unexpected, _}, #state{}}.
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info({tx, {_, Pid}, {request, Request}}, State) ->
    %% send response...
    %?debugFmt("Got message from ~p data ~p~n", [Pid, Data]),
    Response = sip_message:create_response(Request, 486, <<"Busy here">>, <<"anytag">>),
    gen_fsm:sync_send_event(Pid, {response, 486, Response}),
    {noreply, State};
handle_info({tx, _, _TxReq}, State) ->
    % ignore TX messages for now..
    {noreply, State};
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_cast(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
