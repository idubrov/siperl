%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS core implementation
%%%
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-8">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_ua).

-behaviour(gen_server).

%% Custom behaviour
-export([behaviour_info/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% UAC/UAS overridable callbacks
-export([handle_request/3, handle_response/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-spec behaviour_info(callbacks | term()) -> [{Function :: atom(), Arity :: integer()}].
behaviour_info(callbacks) ->
    [{handle_request, 3}, {handle_response, 3} | gen_server:behaviour_info(callbacks)];
behaviour_info(_) -> undefined.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(term()) -> {ok, #sip_ua_state{}}.
init(Module) ->
    {ok, #sip_ua_state{callback = Module}}.

%% @private
-spec handle_call(term(), term(), #sip_ua_state{}) -> any().
handle_call(_Req, _From, State) ->
    {stop, unexpected, State}.

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast(_Req, State) ->
    {stop, unexpected, State}.

%% @private
-spec handle_info(term(), #sip_ua_state{}) -> any().
handle_info({Kind, Msg}, State) when Kind =:= request; Kind =:= response ->
    % Apply request/response pipeline to the incoming message
    % Default pipelines are
    % `sip_ua_pipeline:request_pipeline/0' and `sip_ua_pipeline:response_pipeline/0'
    Pipeline =
        if Kind =:= request -> State#sip_ua_state.uas_pipeline;
           Kind =:= response -> State#sip_ua_state.uac_pipeline
        end,

    FoldFun =
        fun(ProcFun, {next, S}) -> ProcFun(Msg, S);
           (_ProcFun, {noreply, S}) -> {noreply, S};
           (_ProcFun, {reply, Response, S}) ->
                {ok, S2} = sip_uas:send_response(Response, S),
                {noreply, S2}
        end,
    {noreply, State2} = lists:foldl(FoldFun, {next, State}, Pipeline),
    {noreply, State2};

%% @doc Process terminated client transactions, notify UAC
%% @end
handle_info({tx, TxKey, {terminated, Reason}}, State) when is_record(TxKey, sip_tx_client) ->
    {ok, State2} = sip_uac:tx_terminated(TxKey, Reason, State),
    {noreply, State2};

%% @doc Process terminated server transactions, notify UAS
%% @end
handle_info({tx, TxKey, {terminated, Reason}}, State) when is_record(TxKey, sip_tx_server) ->
    {ok, State2} = sip_ua:tx_terminated(TxKey, Reason, State),
    {noreply, State2};

handle_info(_Req, State) ->
    {stop, unexpected, State}.

%% @private
-spec terminate(term(), #sip_ua_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #sip_ua_state{}, term()) -> {ok, #sip_ua_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Default UAC/UAS callbacks

-spec handle_request(binary() | atom(), #sip_message{}, #sip_ua_state{}) -> any().
handle_request(_Method, _Msg, State) ->
    % Apply next request processor
    {next, State}.

-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> any().
handle_response(_UserData, _Msg, State) ->
    % Ignore the response
    {noreply, State}.

