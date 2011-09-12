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

%% Request processing valves
-export([validate_allowed/2, validate_loop/2, validate_required/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").


%% @doc Send response
%%
%% Automatically adds `Allow:' and `Supported:' headers for every reply.
%% @end
-spec send_response(#sip_message{}, #sip_ua_state{}) -> {ok, #sip_ua_state{}}.
send_response(#sip_message{kind = #sip_response{}} = Response,
              #sip_ua_state{allow = Allow, supported = Supported} = State) ->
    % Append Supported and Allow headers
    Resp2 = sip_message:append_header('allow', Allow, Response),
    Resp3 = sip_message:append_header('supported', Supported, Resp2),
    sip_transaction:send_response(Resp3),
    {ok, State}.

% @private
-spec handle_info(term(), #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
handle_info({request, Msg}, State) ->
    Mod = State#sip_ua_state.callback,

    % first, we start server transaction
    sip_transaction:start_server_tx(self(), Msg),

    % parse the URI
    Req = Msg#sip_message.kind,
    URI = sip_uri:parse(Req#sip_request.uri),
    Msg2 = Msg#sip_message{kind = Req#sip_request{uri = URI}},

    % then we apply our standard valves to the message and state
    Result =
        do([pipeline_m ||
            S1 <- validate_allowed(Msg2, State),
            S2 <- validate_loop(Msg2, S1),
            S3 <- validate_required(Msg2, S2),
            S4 <- Mod:handle_request(sip_message:method(Msg2), Msg2, S3),
            S5 <- fail({reply, sip_message:create_response(Msg2, 500), S4}),
            S5]),

    case Result of
        {stop, {reply, Response, S}} ->
            {ok, S2} = send_response(Response, S),
            {stop, {noreply, S2}};
        Other -> Other
    end;

handle_info(_Info, State) ->
    pipeline_m:next(State).

%% Validate message according to the 8.2.1
-spec validate_allowed(#sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
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
-spec validate_loop(#sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
validate_loop(Request, #sip_ua_state{detect_loops = false} = State) ->
    pipeline_m:next(State);
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
-spec validate_required(#sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
validate_required(Request, State) ->
    Supported = State#sip_ua_state.supported,
    IsNotSupported = fun (Ext) -> lists:all(fun (V) -> V =/= Ext end, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header_values('require', Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            pipeline_m:next(State);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = sip_message:create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            pipeline_m:stop({reply, Response2, State})
    end.
