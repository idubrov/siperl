%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Internal UAS core implementation. Clients should use `sip_ua' API.
%%%
%%% Used as a "mixin" in the `sip_ua' gen_server implementation.
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_server).
-compile({parse_transform, do}).

%% Internal API
-export([init/0, create_response/2, create_response/3, send_response/4, handle_request/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type state() :: term().     % Callback module state

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

-spec init() -> ok.
init() ->
    ok.

-spec create_response(#sip_request{}, integer()) -> #sip_response{}.
create_response(Request, Status) ->
    create_response(Request, Status, sip_message:default_reason(Status)).

-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(Request, Status, Reason) ->
    Response = sip_message:create_response(Request, Status, Reason),
    copy_record_route(Request, Response).


%% @doc Initiate a response from the UAS
%% @end
-spec send_response(#sip_request{}, #sip_response{}, module(), state()) -> {ok, state()}.
send_response(Request, Response, Callback, State) when is_record(Request, sip_request), is_record(Response, sip_response) ->
    % Add `To:' header tag automatically (only for non-provisional responses!)
    % FIXME: Do we need different handling of `To:' tag? For example, for 100rel tag should be
    % generated for provisional responses. However, we, probably, do not want to delegate that
    % to the callback module. Maybe we should add tag to incoming request?
    Response2 = add_to_tag(Response),

    ok = do([error_m ||
             sip_message:validate_response(Response2),
             create_dialog(Request, Response2),
             internal_send(Request, Response2, Callback, State)]),
    {ok, State}.


%% @private
-spec handle_request(#sip_request{}, module(), state()) -> {ok, state()}.
handle_request(Request, Callback, State) ->
    % start server transaction
    {ok, _TxPid} = sip_transaction:start_server_tx(Request, []),

    Result =
        do([error_m ||
            validate_method(Request, Callback, State),
            % FIXME: 8.2.2.1 validation..
            %validate_uris(Request, Callback, State),
            validate_loop(Request, Callback, State),
            validate_required(Request, Callback, State),
            update_dialog(Request, Callback, State)]),

    case Result of
        ok ->
            % pass to the callback
            invoke_callback(Request, Callback, State);
        {error, _Reason} ->
            {noreply, State}
    end.

%% Validate message according to the 8.2.1
-spec validate_method(#sip_request{}, module(), state()) -> error_m:monad(ok).
validate_method(Request, Callback, State) ->
    Allow = Callback:allow(Request, State),
    case lists:member(Request#sip_request.method, Allow) of
        true ->
            error_m:return(ok);
        false ->
            % Send "405 Method Not Allowed"
            ok = internal_send(Request, 405, Callback, State),
            error_m:fail(not_allowed)
    end.

%% Validate message according to the 8.2.2.2
-spec validate_loop(#sip_request{}, module(), state()) -> error_m:monad(ok).
validate_loop(Request, Callback, State) ->
    DetectLoops = Callback:detect_loops(Request, State),
    IsLoop = DetectLoops andalso sip_transaction:is_loop_detected(Request),
    case IsLoop of
        false ->
            error_m:return(ok);
        true ->
            % Send "482 Loop Detected"
            ok = internal_send(Request, 482, Callback, State),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(#sip_request{}, module(), state()) -> error_m:monad(ok).
validate_required(#sip_request{method = 'CANCEL'}, _Callback, _State) ->
    % ignore Require: for CANCEL requests
    error_m:return(ok);
validate_required(Request, Callback, State) ->
    Supported = Callback:supported(Request, State),
    IsNotSupported = fun (Ext) -> not lists:member(Ext, Supported) end,

    %% FIXME: Ignore for ACKs for non-2xx
    Require = sip_message:header_values(require, Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            error_m:return(ok);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = create_response(Request, 420),
            Response2 = sip_message:append_header(unsupported, Unsupported, Response),
            ok = internal_send(Request, Response2, Callback, State),
            error_m:fail(bad_extension)
    end.

-spec update_dialog(#sip_request{}, module(), state()) -> error_m:monad(ok).
%% @doc Validate and update dialog according to the 12.2.2
%% @end
update_dialog(Request, Callback, State) ->
    case sip_message:is_within_dialog(Request) of
        false ->
            error_m:return(ok); % not within dialog
        true ->
            DialogId = sip_dialog:dialog_id(uas, Request),
            CSeq = sip_message:header_top_value(cseq, Request),
            case sip_dialog:update_sequence(DialogId, CSeq#sip_hdr_cseq.sequence) of
                ok ->
                    error_m:return(ok);

                {error, no_dialog} ->
                    % Send "481 Call/Transaction Does Not Exist"
                    ok = internal_send(Request, 481, Callback, State),
                    error_m:fail(no_dialog);

                {error, out_of_order} ->
                    % Send "500 Server Internal Error"
                    ok = internal_send(Request, 500, Callback, State),
                    error_m:fail(no_dialog)
            end
    end.

invoke_callback(Request, Callback, State) ->
    Method = Request#sip_request.method,
    case Callback:Method(Request, State) of
        {noreply, State2} ->
            {noreply, State2};
        {reply, Response, State2} ->
            {ok, State2} =
                send_response(Request, Response, Callback, State2),
            {noreply, State2}
    end.


%% @doc Create dialog if response is dialog creating response
%% @end
create_dialog(#sip_request{} = Request, #sip_response{status = Status} = Response) ->
    case sip_message:is_dialog_establishing(Request) of
        true when Status >= 200, Status =< 299 ->
            sip_dialog:create_dialog(uas, Request, Response);
        _Other ->
            error_m:return(ok)
    end.

%% @doc Copy Record-Route for dialog-establishing responses
%% @end
copy_record_route(Request, #sip_response{status = Status} = Response) ->
    case sip_message:is_dialog_establishing(Request) of
        true when Status >= 200, Status =< 299 ->
            % Copy all Record-Route headers
            RecordRoutes = [{'record-route', Value} ||
                            {'record-route', Value} <- Request#sip_request.headers],
            Response#sip_response{headers = Response#sip_response.headers ++ RecordRoutes};
        _Other ->
            Response
    end.

%% @doc Append `To:' header tag if not present and response is not provisional response
%% @end
add_to_tag(#sip_response{status = Status} = Response) when Status >= 100, Status =< 199 -> Response;
add_to_tag(Response) ->
    Fun =
        fun(#sip_hdr_address{params = Params} = To) ->
                case lists:keyfind(tag, 1, Params) of
                    false ->
                        ToTag = sip_idgen:generate_tag(),
                        To#sip_hdr_address{params = [{tag, ToTag} | Params]};
                    {tag, _ToTag} -> To
                end
        end,
    sip_message:update_top_header(to, Fun, Response).

%% @doc Send with `Server:', `Allow:' and `Supported:' headers added to the response
%% @end
internal_send(Request, Status, Callback, State) when is_integer(Status) ->
    internal_send(Request, create_response(Request, Status), Callback, State);
internal_send(Request, Response, Callback, State) ->
    % Append Supported, Allow and Server headers, but only if they were not
    % added explicitly
    Fun =
        fun(Header, Acc) ->
                HeaderValue = Callback:Header(Request, State),
                sip_message:append_header(Header, HeaderValue, Acc)
        end,
    Response2 = lists:foldl(Fun, Response, [allow, supported, server]),

    {ok, _TxPid} = sip_transaction:send_response(Response2),
    ok.
