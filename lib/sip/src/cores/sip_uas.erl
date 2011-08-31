%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS request processing behaviour
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_uas).
-compile({parse_transform, do}).

%% API
-export([send_response/2, handle_info/2]).

%% Request processing
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

% @private
-spec handle_info(term(), #sip_ua_state{}) -> {ok, #sip_ua_state{}}.
handle_info({request, Msg}, State) ->
    Mod = State#sip_ua_state.callback,

    % first, we start server transaction
    sip_transaction:start_server_tx(self(), Msg),

    % then we apply our standard valves to the message and state
    Result =
        do([pipeline_m ||
            S1 <- validate_allowed(Msg, State),
            S2 <- validate_loop(Msg, S1),
            S3 <- validate_required(Msg, S2),
            S4 <- Mod:handle_request(sip_message:method(Msg), Msg, S3),
            S5 <- fail({reply, sip_message:create_response(Msg, 500), S4}),
            S5]),

    case Result of
        {stop, {reply, Response, S}} ->
            {ok, S2} = send_response(Response, S),
            {stop, {noreply, S2}};
        Other -> Other
    end;

handle_info(_Info, State) ->
    pipeline_m:next(State).

%% Request handling
start_server_tx(Request, State) ->
    sip_transaction:start_server_tx(self(), Request),
    pipeline_m:next(State).

%% Validate message according to the 8.2.1
validate_allowed(Request, State) ->
    Method = Request#sip_message.kind#sip_request.method,
    Allow = State#sip_ua_state.allow,
    Contains = lists:any(fun (V) -> V =:= Method end, Allow),
    case Contains of
        true ->
            pipeline_m:next(State);
        false ->
            % Send "405 Method Not Allowed"
            Response = sip_message:create_response(Request, 405),
            pipeline_m:stop({reply, Response, State})
    end.

%% Validate message according to the 8.2.2.2
validate_loop(Request, State) ->
    case sip_transaction:is_loop_detected(Request) of
        false ->
            pipeline_m:next(State);
        true ->
            % Send "482 Loop Detected"
            Response = sip_message:create_response(Request, 482),
            pipeline_m:stop({reply, Response, State})
    end.

%% Validate message according to the 8.2.2.3
validate_required(Request, State) ->
    Supported = State#sip_ua_state.supported,
    IsNotSupported = fun (Ext) -> lists:all(fun (V) -> V =/= Ext end, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header('require', Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            pipeline_m:next(State);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = sip_message:create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            pipeline_m:stop({reply, Response2, State})
    end.

%% Invoke request handler
invoke_handler(Request, #sip_ua_state{callback = Mod} = State) ->
    Method = sip_message:method(Request),
    case Mod:handle_request(Method, Request, State) of
        {next, S} -> pipeline_m:next(S);
        Other -> pipeline_m:stop(Other)
    end.

default_response(Request, State) ->
    % Send 'Server Internal Error' by default
    Response = sip_message:create_response(Request, 500),
    pipeline_m:stop({reply, Response, State}).
