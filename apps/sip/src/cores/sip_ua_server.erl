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
-export([init/1, create_response/2, create_response/3, send_response/3, handle_request/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% Process dictionary key for loop options
-define(DETECT_LOOPS, sip_ua_server_detect_loops).

-type state() :: term().     % Callback module state

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

-spec init(list()) -> ok.
init(Opts) ->
    erlang:put(?DETECT_LOOPS, not proplists:get_bool(no_detect_loops, Opts)),
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
-spec send_response(#sip_request{}, #sip_response{}, module()) -> ok | {error, Reason :: term()}.
send_response(Request, Response, Callback) when is_record(Request, sip_request), is_record(Response, sip_response) ->
    % Add `To:' header tag automatically (only for non-provisional responses!)
    % FIXME: Do we need different handling of `To:' tag? For example, for 100rel tag should be
    % generated for provisional responses. However, we, probably, do not want to delegate that
    % to the callback module. Maybe we should add tag to incoming request?
    Response2 = add_to_tag(Response),

    do([error_m ||
        sip_message:validate_response(Response2),
        create_dialog_session(Request, Response2),
        internal_send(Request, Response2, Callback)]).

%% @private
-spec handle_request(#sip_request{}, module(), state()) -> {noreply, state()}.
handle_request(Request, Callback, State) ->
    % start server transaction
    {ok, _TxPid} = sip_transaction:start_server_tx(Request, []),

    Result =
        do([error_m ||
            update_remote_seq(Request, Callback),
            validate_method(Request, Callback),
            % FIXME: 8.2.2.1 validation..
            %validate_uris(Request, Callback, State),
            validate_loop(Request, Callback),
            validate_required(Request, Callback),
            handle_session(Request, Callback),
            handle_bye_request(Request, Callback)]),

    case Result of
        ok ->
            % pass to the callback
            invoke_callback(Request, Callback, State);
        {error, _Reason} ->
            {noreply, State}
    end.

%% Validate message according to the 8.2.1
-spec validate_method(#sip_request{}, module()) -> error_m:monad(ok).
validate_method(Request, Callback) ->
    Allow = Callback:allow(Request),
    case lists:member(Request#sip_request.method, Allow) of
        true ->
            error_m:return(ok);
        false ->
            % Send "405 Method Not Allowed"
            ok = internal_send(Request, 405, Callback),
            error_m:fail(not_allowed)
    end.

%% Validate message according to the 8.2.2.2
-spec validate_loop(#sip_request{}, module()) -> error_m:monad(ok).
validate_loop(Request, Callback) ->
    IsLoop = detect_loops() andalso sip_transaction:is_loop_detected(Request),
    case IsLoop of
        false ->
            error_m:return(ok);
        true ->
            % Send "482 Loop Detected"
            ok = internal_send(Request, 482, Callback),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(#sip_request{}, module()) -> error_m:monad(ok).
validate_required(#sip_request{method = 'CANCEL'}, _Callback) ->
    % ignore Require: for CANCEL requests
    error_m:return(ok);
validate_required(Request, Callback) ->
    Supported = Callback:supported(Request),
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
            ok = internal_send(Request, Response2, Callback),
            error_m:fail(bad_extension)
    end.

-spec update_remote_seq(#sip_request{}, module()) -> error_m:monad(ok).
%% @doc Validate and update remote sequence according to the 12.2.2
%% @end
update_remote_seq(Request, Callback) ->
    case sip_message:is_within_dialog(Request) of
        false ->
            error_m:return(ok); % not within dialog
        true ->
            DialogId = sip_dialog:dialog_id(uas, Request),
            CSeq = sip_message:header_top_value(cseq, Request),
            case sip_dialog:update_remote_seq(DialogId, CSeq#sip_hdr_cseq.sequence) of
                ok ->
                    error_m:return(ok);

                {error, no_dialog} ->
                    % Send "481 Call/Transaction Does Not Exist"
                    ok = internal_send(Request, 481, Callback),
                    error_m:fail(no_dialog);

                {error, out_of_order} ->
                    % Send "500 Server Internal Error"
                    ok = internal_send(Request, 500, Callback),
                    error_m:fail(out_of_order)
            end
    end.

-spec handle_bye_request(#sip_request{}, module()) -> error_m:monad(ok).
%% @doc Default handling of `BYE' requests
%% @end
handle_bye_request(#sip_request{method = 'BYE'} = Request, Callback) ->
    DialogId = sip_dialog:dialog_id(uas, Request),
    Status =
        case sip_dialog:terminate_dialog(DialogId) of
            ok ->
                % this will terminate session as well
                200; % Ok
            {error, no_dialog} ->
                481 % Call/Transaction Does Not Exist
        end,
    ok = internal_send(Request, Status, Callback),

    % FIXME: The UAS MUST still respond to any pending requests received for that dialog.
    % How should we do that? Keep a dialog for some time?
    error_m:fail(processed);
handle_bye_request(_Request, _Callback) ->
    error_m:return(ok).

%% @doc Handle offer/answer from the remote side
%% @end
-spec handle_session(#sip_request{}, module()) -> error_m:monad(ok).
handle_session(Request, _Callback) ->
    case sip_offer_answer:validate_request(Request) of
        ok -> error_m:return(ok); % nothing of interest
        Kind ->
            case sip_message:session(Request) of
                false ->
                    error_m:fail(session_expected);
                #sip_session_desc{} = SessionDesc ->
                    io:format("FIXME: UAS SESSION ~p ~p~n", [Kind, SessionDesc])
            end
    end.

invoke_callback(Request, Callback, State) ->
    Method = Request#sip_request.method,
    case Callback:Method(Request, State) of
        {default, State2} ->
            % delegate to default handler
            % note: sip_ua_default should never return {default, State}
            invoke_callback(Request, sip_ua_default, State2);
        {noreply, State2} ->
            {noreply, State2};
        {reply, Response, State2} ->
            ok = send_response(Request, Response, Callback),
            {noreply, State2}
    end.


%% @doc Create dialog and session if response 2xx and request is 'INVITE' not within a dialog
%% @end
create_dialog_session(#sip_request{} = Request, #sip_response{status = Status} = Response)
  when Status >= 200, Status =< 299, Request#sip_request.method =:= 'INVITE' ->
    case sip_message:is_within_dialog(Request) of
        false ->
            {ok, DialogId} = sip_dialog:create_dialog(uas, Request, Response),
            ok = sip_dialog:create_session(DialogId), % session is created upon 2xx response sending
            error_m:return(ok);
        true ->
            % Response to re-INVITE
            error_m:return(ok)
    end;

create_dialog_session(_Request, _Response) ->
    error_m:return(ok).

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
internal_send(Request, Status, Callback) when is_integer(Status) ->
    internal_send(Request, create_response(Request, Status), Callback);
internal_send(Request, Response, Callback) ->
    % Append Supported, Allow and Server headers, but only if they were not
    % added explicitly
    Fun =
        fun(Header, Acc) ->
                HeaderValue = Callback:Header(Request),
                sip_message:append_header(Header, HeaderValue, Acc)
        end,
    Response2 = lists:foldl(Fun, Response, [allow, supported, server]),

    {ok, _TxPid} = sip_transaction:send_response(Response2),
    ok.

%%-----------------------------------------------------------------
%% State management (functions that use process dictionary)
%%-----------------------------------------------------------------

detect_loops() ->
    erlang:get(?DETECT_LOOPS).