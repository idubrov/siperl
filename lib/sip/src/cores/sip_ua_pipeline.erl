%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS message processing pipeline
%%%
%%% XXX: Circular dependencies between this module and `sip_ua'.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_ua_pipeline).

%% API
-export([invoke/2, request_pipeline/0, response_pipeline/0]).

%% Request processing
-export([start_server_tx/1, validate_allowed/1, validate_loop/1, validate_required/1, handle_request/1]).
-export([validate_vias/1, handle_response/1]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

request_pipeline() ->
    [fun start_server_tx/1, fun validate_allowed/1, fun validate_loop/1, fun validate_required/1, fun handle_request/1].

response_pipeline() ->
    [fun validate_vias/1, fun handle_response/1].

invoke(Pipeline, Acc0) ->
    % Apply pipeline functions from the list until {stop, Result} is returned.
    Fun =
        fun(_ProcFun, {stop, Acc}) -> {stop, Acc};
           (ProcFun, {next, Acc}) -> ProcFun(Acc)
        end,
    lists:foldl(Fun, {next, Acc0}, Pipeline).

%% Response handling

%% Validate message according to the 8.1.3.3
validate_vias({Msg, State}) ->
    Count = length(sip_message:header('via', Msg)),
    case Count of
        1 ->
            {next, {Msg, State}};

        _Other ->
            % discard response, too much/few Via's
            error_logger:warning_report(['message_discarded',
                                         {reason, wrong_vias},
                                         {msg, Msg}]),
            {stop, {noreply, State}}
    end.

%% Invoke response handler
handle_response({Msg, #sip_ua_state{callback = Mod} = State}) ->
    TxKey = sip_transaction:tx_key(client, Msg),
    ReqInfo = dict:fetch(TxKey, State#sip_ua_state.requests),
    UserData = ReqInfo#req_info.user_data,

    % handle response from transaction layer
    {stop, Mod:handle_response(UserData, Msg, State)}.

%% Request handling
start_server_tx({Msg, State}) ->
    sip_transaction:start_server_tx(self(), Msg),
    {next, {Msg, State}}.

%% Validate message according to the 8.2.1
validate_allowed({Msg, State} = Value) ->
    Method = Msg#sip_message.kind#sip_request.method,
    Allow = State#sip_ua_state.allow,
    Contains = lists:any(fun (V) -> V =:= Method end, Allow),
    case Contains of
        true -> {next, Value};
        false ->
                % Send "405 Method Not Allowed"
                Resp = sip_message:create_response(Msg, 405),
                sip_ua:send_response(Resp),
                {stop, {noreply, State}}
    end.

%% Validate message according to the 8.2.2.2
validate_loop({Msg, State} = Value) ->
    case sip_transaction:is_loop_detected(Msg) of
        false -> {next, Value};
        true ->
                % Send "482 Loop Detected"
                Resp = sip_message:create_response(Msg, 482),
                sip_ua:send_response(Resp),
                {stop, {noreply, State}}
    end.

%% Validate message according to the 8.2.2.3
validate_required({Msg, State} = Value) ->
    Supported = State#sip_ua_state.supported,
    IsNotSupported = fun (Ext) -> lists:all(fun (V) -> V =/= Ext end, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header('require', Msg),
    case lists:filter(IsNotSupported, Require) of
        [] -> {next, Value};
        Unsupported ->
                % Send "420 Bad Extension"
                Resp = sip_message:create_response(Msg, 420),
                Resp2 = sip_message:append_header('unsupported', Unsupported, Resp),
                sip_ua:send_response(Resp2),
                {stop, {noreply, State}}
    end.

%% Invoke request handler
handle_request({Msg, #sip_ua_state{callback = Mod} = State}) ->
    Method = sip_message:method(Msg),
    {stop, Mod:handle_request(Method, Msg, State)}.
