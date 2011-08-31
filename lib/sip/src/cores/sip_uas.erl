%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS request processing behaviour
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_uas).

%% API
-export([send_response/2, tx_terminated/3]).

%% Response processing
-export([pipeline/0]).
-export([start_server_tx/2, validate_allowed/2, validate_loop/2, validate_required/2, invoke_handler/2, default_response/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

send_response(Response, #sip_ua_state{allow = Allow, supported = Supported} = State) ->
    % Append Supported and Allow headers
    Resp2 = sip_message:append_header('allow', Allow, Response),
    Resp3 = sip_message:append_header('supported', Supported, Resp2),
    sip_transaction:send_response(Resp3),
    {ok, State}.

tx_terminated(_TxKey, normal, State) ->
    {ok, State}.

pipeline() ->
    [fun start_server_tx/2,
     fun validate_allowed/2,
     fun validate_loop/2,
     fun validate_required/2,
     fun invoke_handler/2,
     fun default_response/2].

%% Request handling
start_server_tx(Request, State) ->
    sip_transaction:start_server_tx(self(), Request),
    {next, State}.

%% Validate message according to the 8.2.1
validate_allowed(Request, State) ->
    Method = Request#sip_message.kind#sip_request.method,
    Allow = State#sip_ua_state.allow,
    Contains = lists:any(fun (V) -> V =:= Method end, Allow),
    case Contains of
        true ->
            {next, State};
        false ->
            % Send "405 Method Not Allowed"
            Response = sip_message:create_response(Request, 405),
            {reply, Response, State}
    end.

%% Validate message according to the 8.2.2.2
validate_loop(Request, State) ->
    case sip_transaction:is_loop_detected(Request) of
        false ->
            {next, State};
        true ->
            % Send "482 Loop Detected"
            Response = sip_message:create_response(Request, 482),
            {reply, Response, State}
    end.

%% Validate message according to the 8.2.2.3
validate_required(Request, State) ->
    Supported = State#sip_ua_state.supported,
    IsNotSupported = fun (Ext) -> lists:all(fun (V) -> V =/= Ext end, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header('require', Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            {next, State};
        Unsupported ->
            % Send "420 Bad Extension"
            Response = sip_message:create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            {reply, Response2, State}
    end.

%% Invoke request handler
invoke_handler(Request, #sip_ua_state{callback = Mod} = State) ->
    Method = sip_message:method(Request),
    Mod:handle_request(Method, Request, State).

default_response(Request, State) ->
    % Send 'Server Internal Error' by default
    Response = sip_message:create_response(Request, 500),
    {reply, Response, State}.
