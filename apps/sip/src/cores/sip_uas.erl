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
-export([start_link/2, create_response/2, create_response/3, send_response/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(req_info, {request          :: #sip_request{},
                   tag              :: binary()}).       % Tag for the `To:' header
-record(state, {callback      :: module(),
                requests = [] :: [{#sip_tx_server{}, #req_info{}}],
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
-spec send_response(pid() | atom(), #sip_response{}) -> ok.
send_response(UAS, Response) when is_record(Response, sip_response) ->
    gen_server:cast(UAS, {send_response, Response}),
    ok.

-spec create_response(#sip_request{}, integer()) -> #sip_response{}.
create_response(Request, Status) ->
    create_response(Request, Status, sip_message:default_reason(Status)).

-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(Request, Status, Reason) ->
    Response = sip_message:create_response(Request, Status, Reason),
    Response2 = copy_record_route(Request, Response),
    add_to_tag(Response2, Status).

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
handle_cast({send_response, Response}, State) ->
    {ok, State2} = do_send_response(Response, State),
    {noreply, State2};
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
        {reply, Response, Context} ->
            {ok, State2} = do_send_response(Response, State),
            {noreply, State2#state{context = Context}};
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
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % validate message
    Result =
        do([error_m ||
            S1 <- validate_method(Request, State),
            S2 <- validate_loop(Request, S1),
            S3 <- validate_required(Request, S2),
            S4 <- record_request(TxKey, Request, S3),
            process_cancel(Request, S4)]),
    case Result of
        {ok, State2} ->
            invoke_callback(Request, State2);
        {error, {State2, _Reason}} ->
            {ok, State2}
    end.

%% Validate message according to the 8.2.1
-spec validate_method(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_method(Request, #state{callback = Callback} = State) ->
    Allow = Callback:allow(Request, State#state.context),
    case lists:member(Request#sip_request.method, Allow) of
        true ->
            error_m:return(State);
        false ->
            % Send "405 Method Not Allowed"
            Response = create_response(Request, 405),
            {ok, State2} = do_send_response(Response, State),
            error_m:fail({State2, not_allowed})
    end.

%% Validate message according to the 8.2.2.2
-spec validate_loop(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_loop(Request, #state{callback = Callback} = State) ->
    DetectLoops = Callback:detect_loops(Request, State#state.callback),
    IsLoop = DetectLoops andalso sip_transaction:is_loop_detected(Request),
    case IsLoop of
        false ->
            error_m:return(State);
        true ->
            % Send "482 Loop Detected"
            Response = create_response(Request, 482),
            {ok, State2} = do_send_response(Response, State),
            error_m:fail({State2, loop_detected})
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_required(#sip_request{method = 'CANCEL'}, State) ->
    % ignore Require: for CANCEL requests
    error_m:return(State);
validate_required(Request, #state{callback = Callback} = State) ->
    Supported = Callback:supported(Request, State#state.context),
    IsNotSupported = fun (Ext) -> not lists:member(Ext, Supported) end,

    %% FIXME: Ignore for ACKs for non-2xx
    Require = sip_message:header_values(require, Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            error_m:return(State);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            {ok, State2} = do_send_response(Response2, State),
            error_m:fail({State2, bad_extension})
    end.

-spec do_send_response(#sip_response{}, #state{}) -> {ok, #state{}} | {error, Reason :: term()}.
do_send_response(#sip_response{status = Status} = Response, State) ->
    TxKey = sip_transaction:tx_key(server, Response),
    case lists:keyfind(TxKey, 1, State#state.requests) of
        {_, ReqInfo} ->
            case do_pre_send(ReqInfo#req_info.request, Response) of
                ok ->
                    % Append Supported, Allow and Server headers, but only if they were not
                    % added explicitly
                    Response2 = add_from_callback([allow, supported, server], State, ReqInfo#req_info.request, Response),

                    % send
                    {ok, _TxKey} = sip_transaction:send_response(Response2),
                    if Status >= 100, Status =< 199 -> {ok, State};
                    true ->
                        % final response
                        Requests = lists:keydelete(TxKey, 1, State#state.requests),
                        {ok, State#state{requests = Requests}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {ok, State} % FIXME: ....Ignore for now....
    end.

process_cancel(#sip_request{method = 'CANCEL'} = Request, State) ->
    % 9.2, process CANCEL requests
    {State2, Status} =
        case sip_transaction:lookup_cancel(Request) of
            false ->
                {State, 481};    % Call/Transaction Does Not Exist
            {ok, TxKey} ->
                {ok, S} = cancel_transaction(TxKey, State),
                {S, 200}         % Ok
        end,
    % Reply to CANCEL
    Response = create_response(Request, Status),
    {ok, State3} = do_send_response(Response, State2),

    error_m:fail({State3, cancel}); % do not continue to UAS callback
process_cancel(_Request, State) ->
    error_m:return(State).

invoke_callback(Request, #state{callback = Callback} = State) ->
    Method = Request#sip_request.method,
    case Callback:Method(Request, State#state.context) of
        {noreply, Context} ->
            {ok, State#state{context = Context}};
        {reply, Response, Context} ->
            {ok, State2} = do_send_response(Response, State),
            {ok, State2#state{context = Context}}
    end.


%% @doc Validates response, creates dialog if response is dialog creating response
%% @end
do_pre_send(Request, Response) ->
    case sip_dialog:is_dialog_establishing(Request, Response) of
        true ->
            case sip_message:validate_dialog_response(Request, Response) of
                ok ->
                    sip_dialog:create_dialog(uas, Request, Response);
                {error, Reason} -> {error, Reason}
            end;
        false ->
            sip_message:validate_response(Response)
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
add_to_tag(Response, Status) when Status >= 100, Status =< 199 -> Response;
add_to_tag(Response, _Status) ->
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

add_from_callback([], #state{} = _State, _Request, Response) -> Response;
add_from_callback([Header | Rest], #state{callback = Callback, context = Context} = State, Request, Response) ->
    case sip_message:has_header(Header, Response) of
        true -> Response;
        false ->
            Response2 = sip_message:append_header(Header, Callback:Header(Request, Context), Response),
            add_from_callback(Rest, State, Request, Response2)
    end.

record_request(TxKey, Request, State) ->
    ReqInfo = #req_info{request = Request},
    error_m:return(State#state{requests = [{TxKey, ReqInfo} | State#state.requests]}).

cancel_transaction(TxKey, State) ->
    case lists:keyfind(TxKey, 1, State#state.requests) of
        false -> {ok, State}; % Final response was already sent, do nothing
        {_, ReqInfo} ->
            Response = create_response(ReqInfo#req_info.request, 487),
            do_send_response(Response, State)
    end.
