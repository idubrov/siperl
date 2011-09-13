%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS core implementation
%%%
%%% This module could be used as base implementation of UAC/UAS gen_server.
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-8">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua).
-compile({parse_transform, do}).

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
init(_Args) ->
    {ok, #sip_ua_state{}}.

%% @private
-spec handle_call(term(), term(), #sip_ua_state{}) -> any().
handle_call(_Req, _From, State) ->
    {stop, unexpected, State}.

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast(_Req, State) ->
    {stop, unexpected, State}.

%% @private
-spec handle_info(term(), #sip_ua_state{}) -> {ok, #sip_ua_state{}}.
handle_info(Info, State) ->
    {stop, Result} =
        do([pipeline_m ||
            S1 <- sip_uas:handle_info(Info, State),
            S2 <- sip_uac:handle_info(Info, S1),
            % terminate process with `unexpected' reason
            S3 <- fail({stop, {unexpected, Info}, S2}),
            S3]),
    Result.

%% @private
-spec terminate(term(), #sip_ua_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #sip_ua_state{}, term()) -> {ok, #sip_ua_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Default UAC/UAS callbacks

-spec handle_request(binary() | atom(), #sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
handle_request(_Method, _Msg, State) ->
    % Apply next request processor
    pipeline_m:next(State).

-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
handle_response(_UserData, _Msg, State) ->
    % Ignore the response
    pipeline_m:next(State).

