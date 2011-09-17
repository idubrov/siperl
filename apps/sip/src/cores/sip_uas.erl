%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS request processing behaviour
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_uas).
-behaviour(gen_server).
-compile({parse_transform, do}).

%% API
-export([start_link/2, create_response/2, create_response/3, send_response/3]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(state, {callback      :: module(),
                context       :: term()}).      % Callback context

-type gen_from() :: {pid(), term()}.

%% API

%% @doc Creates UAS as part of the supervision tree.
%% @end
-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Callback, Param) when is_atom(Callback) ->
    gen_server:start_link(?MODULE, {Callback, Param}, []).

%% @doc Initiate a response from the UAS
%% @end
-spec send_response(pid() | atom(), #sip_request{}, #sip_response{}) -> ok.
send_response(UAS, Request, Response) when is_record(Response, sip_response) ->
    gen_server:cast(UAS, {send_response, Request, Response}),
    ok.

-spec create_response(#sip_request{}, integer()) -> #sip_response{}.
create_response(Request, Status) ->
    create_response(Request, Status, sip_message:default_reason(Status)).

-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(Request, Status, Reason) ->
    Response = sip_message:create_response(Request, Status, Reason),
    copy_record_route(Request, Response).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({module(), term()}) -> {ok, #state{}}.
init({Callback, Param}) ->
    {ok, Context} = Callback:init(Param),
    IsApplicable =
        fun(#sip_response{}) -> false; % UAS never handles responses
           (#sip_request{} = Msg) -> Callback:is_applicable(Msg)
        end,
    sip_cores:register_core(#sip_core_info{is_applicable = IsApplicable}),
    {ok, #state{callback = Callback,
                context = Context}}.

%% @private
-spec handle_call(term(), gen_from(), #state{}) -> {stop, {unexpected, term()}, #state{}}.
handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast({send_response, #sip_response{}}, #state{}) -> {noreply, #state{}}.
handle_cast({send_response, Request, Response}, State) ->
    ok = do_send_response(Request, Response, State),
    {noreply, State};
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info({request, #sip_request{}}, #state{}) -> {noreply, #state{}}.
handle_info({request, Request}, State) ->
    {ok, State2} = do_request(Request, State),
    {noreply, State2};
handle_info({tx, _TxKey, {terminated, _Reason}}, State) ->
    % FIXME: should we handle terminated transactions?
    {noreply, State};
handle_info(Info, #state{callback = Callback} = State) ->
    case Callback:handle_info(Info, State#state.context) of
        {noreply, Context} ->
            {noreply, State#state{context = Context}};
        {stop, Reason, Context} ->
            {stop, Reason, State#state{context = Context}}
    end.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

do_request(Request, State) ->
    % start server transaction
    {ok, _TxKey} = sip_transaction:start_server_tx(self(), Request),

    % validate message
    Result =
        do([error_m ||
            validate_method(Request, State),
            validate_loop(Request, State),
            validate_required(Request, State)]),
    case Result of
        ok -> invoke_callback(Request, State);
        {error, _Reason} -> {ok, State}
    end.

%% Validate message according to the 8.2.1
-spec validate_method(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_method(Request, #state{callback = Callback} = State) ->
    Allow = Callback:allow(Request, State#state.context),
    case lists:member(Request#sip_request.method, Allow) of
        true ->
            error_m:return(ok);
        false ->
            % Send "405 Method Not Allowed"
            % FIXME: log request?
            ok = internal_send(Request, 405, State),
            error_m:fail(not_allowed)
    end.

%% Validate message according to the 8.2.2.2
-spec validate_loop(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_loop(Request, #state{callback = Callback} = State) ->
    DetectLoops = Callback:detect_loops(Request, State#state.callback),
    IsLoop = DetectLoops andalso sip_transaction:is_loop_detected(Request),
    case IsLoop of
        false ->
            error_m:return(ok);
        true ->
            % Send "482 Loop Detected"
            % FIXME: log request?
            ok = internal_send(Request, 482, State),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_required(#sip_request{method = 'CANCEL'}, _State) ->
    % ignore Require: for CANCEL requests
    error_m:return(ok);
validate_required(Request, #state{callback = Callback} = State) ->
    Supported = Callback:supported(Request, State#state.context),
    IsNotSupported = fun (Ext) -> not lists:member(Ext, Supported) end,

    %% FIXME: Ignore for ACKs for non-2xx
    Require = sip_message:header_values(require, Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            error_m:return(ok);
        Unsupported ->
            % Send "420 Bad Extension"
            % FIXME: log request?
            Response = create_response(Request, 420),
            Response2 = sip_message:append_header(unsupported, Unsupported, Response),
            ok = internal_send(Request, Response2, State),
            error_m:fail(bad_extension)
    end.


do_send_response(Request, Response, State) ->
    % Add `To:' header tag automatically (only for non-provisional responses!)
    % FIXME: Do we need different handling of `To:' tag? For example, for 100rel tag should be
    % generated for provisional responses. However, we, probably, do not want to delegate that
    % to the callback module. Maybe we should add tag to incoming request?
    Response2 = add_to_tag(Response),

    do([error_m ||
        sip_message:validate_response(Response2),
        create_dialog(Request, Response2),
        internal_send(Request, Response2, State)]).

invoke_callback(Request, #state{callback = Callback} = State) ->
    Method = Request#sip_request.method,
    case Callback:Method(Request, State#state.context) of
        {noreply, Context} ->
            {ok, State#state{context = Context}};
        {reply, Response, Context} ->
            ok = do_send_response(Request, Response, State),
            {ok, State#state{context = Context}}
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
copy_record_route(Request, Response) ->
    case sip_dialog:is_dialog_establishing(Request, Response) of
        true ->
            % Copy all Record-Route headers
            RecordRoutes = [{'record-route', Value} ||
                            {'record-route', Value} <- Request#sip_request.headers],
            Response#sip_response{headers = Response#sip_response.headers ++ RecordRoutes};
        false ->
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
internal_send(Request, Status, State) when is_integer(Status) ->
    internal_send(Request, create_response(Request, Status), State);
internal_send(Request, Response, State) ->
    % Append Supported, Allow and Server headers, but only if they were not
    % added explicitly
    Response2 = add_from_callback([allow, supported, server], State, Request, Response),

    {ok, _TxKey} = sip_transaction:send_response(Response2),
    ok.

%% @doc Add header, retrieved from the callback, to the response
%% @end
add_from_callback([], #state{} = _State, _Request, Response) -> Response;
add_from_callback([Header | Rest], #state{callback = Callback, context = Context} = State, Request, Response) ->
    case sip_message:has_header(Header, Response) of
        true -> Response;
        false ->
            Response2 = sip_message:append_header(Header, Callback:Header(Request, Context), Response),
            add_from_callback(Rest, State, Request, Response2)
    end.
