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
-export([new/1, send_response/2, process_request/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(uas, {options = [] :: [{atom(), any()}]}).

-spec new([{'ALLOW', [sip_name()]} | {'OPTIONS', [sip_name()]} | no_detect_loops]) -> #uas{}.
new(Options) when is_list(Options) ->
    #uas{options = Options}.

%% @doc Send response
%%
%% Automatically adds `Allow:' and `Supported:' headers for every reply.
%% @end
-spec send_response(#uas{}, sip_message()) -> ok.
send_response(#uas{options = Options}, Response)
  when is_record(Response, sip_response) ->
    % Append Supported and Allow headers
    Resp2 = sip_message:append_header(allow, proplists:get_value(allow, Options, []), Response),
    Resp3 = sip_message:append_header(supported, proplists:get_value(supported, Options, []), Resp2),
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
validate_allowed(Request, #uas{options = Options} = UAS) ->
    Method = Request#sip_request.method,
    Allow = proplists:get_value(allow, Options, []),
    Contains = lists:member(Method, Allow),
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
validate_loop(Request, #uas{options = Options} = UAS) ->
    IsLoop = (not proplists:get_bool(no_detect_loops, Options)) andalso
             sip_transaction:is_loop_detected(Request),
    case IsLoop of
        false ->
            error_m:return(ok);
        true ->
            % Send "482 Loop Detected"
            Response = sip_message:create_response(Request, 482),
            send_response(Response, UAS),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(sip_message(), #uas{}) -> error_m:monad(ok).
validate_required(Request, #uas{options = Options} = UAS) ->
    Supported = proplists:get_value(supported, Options, []),
    IsNotSupported = fun (Ext) -> not lists:member(Ext, Supported) end,

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
