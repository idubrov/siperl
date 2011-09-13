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
-export([new/3, send_response/2, process_request/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(uas, {callback :: module(), % Client callback module
                allow = [] :: [atom()],      % List of allowed methods
                supported = [] :: [atom()],  % List of supported extensions
                detect_loops = false :: boolean() % Enable loop detection (8.2.2.2)
               }).

-spec new(module, [atom()], [atom()]) -> #uas{}.
new(Callback, Allow, Supported) ->
    #uas{callback = Callback, allow = Allow, supported = Supported}.


%% @doc Send response
%%
%% Automatically adds `Allow:' and `Supported:' headers for every reply.
%% @end
-spec send_response(#uas{}, sip_message()) -> ok.
send_response(#uas{allow = Allow, supported = Supported}, Response)
  when is_record(Response, sip_response) ->
    % Append Supported and Allow headers
    Resp2 = sip_message:append_header('allow', Allow, Response),
    Resp3 = sip_message:append_header('supported', Supported, Resp2),
    sip_transaction:send_response(Resp3),
    ok.

% @private
-spec process_request(#uas{}, sip_message()) -> ok | {error, Reason :: term()}.
process_request(UAS, Msg) when is_record(UAS, uas), is_record(Msg, sip_request) ->
    % start server transaction
    sip_transaction:start_server_tx(self(), Msg),

    % validate message
    do([error_m ||
        validate_allowed(Msg, UAS),
        validate_loop(Msg, UAS),
        validate_required(Msg, UAS)]).

%% Internal functions

%% Validate message according to the 8.2.1
-spec validate_allowed(sip_message(), #uas{}) -> error_m:monad(ok).
validate_allowed(Request, UAS) ->
    Method = Request#sip_request.method,
    Allow = UAS#uas.allow,
    Contains = lists:any(fun (V) -> V =:= Method end, Allow),
    case Contains of
        true ->
            error_m:return(ok);
        false ->
            % Send "405 Method Not Allowed"
            Response = sip_message:create_response(Request, 405),
            send_response(Response, UAS),
            error_m:fail(not_allowed)
    end.

%% Validate message according to the 8.2.2.2
-spec validate_loop(sip_message(), #uas{}) -> error_m:monad(ok).
validate_loop(_Request, #uas{detect_loops = false}) ->
    error_m:return(ok);
validate_loop(Request, UAS) ->
    case sip_transaction:is_loop_detected(Request) of
        false ->
            error_m:return(UAS);
        true ->
            % Send "482 Loop Detected"
            Response = sip_message:create_response(Request, 482),
            send_response(Response, UAS),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(sip_message(), #uas{}) -> error_m:monad(ok).
validate_required(Request, UAS) ->
    Supported = UAS#uas.supported,
    IsNotSupported = fun (Ext) -> lists:all(fun (V) -> V =/= Ext end, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header_values('require', Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            error_m:return(ok);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = sip_message:create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            send_response(Response2, UAS),
            error_m:fail(bad_extension)
    end.
