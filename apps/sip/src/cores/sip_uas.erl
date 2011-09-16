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

-record(state, {callback :: module(),
                context :: term()}).      % Callback context

-type gen_from() :: {pid(), term()}.

%% API

%% @doc Creates UAS as part of the supervision tree.
%% @end
-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Callback, Context) when is_atom(Callback) ->
    gen_server:start_link(?MODULE, {Callback, Context}, []).

%% @doc Initiate a response from the UAS
%% @end
-spec send_response(pid() | atom(), #sip_request{}, #sip_response{}) -> ok.
send_response(UAS, Request, Response) when is_record(Response, sip_response) ->
    % FIXME: Instead, store unreplied requests somewhere and provide request id...
    gen_server:cast(UAS, {send_response, Request, Response}),
    ok.

-spec create_response(#sip_request{}, integer()) -> #sip_response{}.
create_response(Request, Status) ->
    create_response(Request, Status, sip_message:default_reason(Status)).

-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(Request, Status, Reason) ->
    Response = sip_message:create_response(Request, Status, Reason),
    Response2 =
        case sip_dialog:is_dialog_establishing(Request, Response) of
            true ->
                % Copy all Record-Route headers
                RecordRoutes = [{'record-route', Value} ||
                                {'record-route', Value} <- Request#sip_request.headers],
                Response#sip_response{headers = Response#sip_response.headers ++ RecordRoutes};
            false ->
                Response
        end,

    % Add `To' Tag if not present
    Fun = fun(#sip_hdr_address{params = Params} = Addr) ->
                  case lists:keyfind(tag, 1, Params) of
                      false ->
                          ToTag = sip_idgen:generate_tag(),
                          Addr#sip_hdr_address{params = [{tag, ToTag} | Params]};
                      {tag, _ToTag} -> Addr
                  end
          end,
    sip_message:update_top_header(to, Fun, Response2).

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
handle_info(Info, State) ->
    {stop, {unexpected, Info}, State}.

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
            validate_allowed(Request, State),
            validate_loop(Request, State),
            validate_required(Request, State)]),
    case Result of
        ok ->
            do_handle_request(Request, State);
        {error, _Reason} ->
            {ok, State}
    end.

%% Validate message according to the 8.2.1
-spec validate_allowed(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_allowed(Request, #state{callback = Callback} = State) ->
    Allow = Callback:allowed_methods(Request, State#state.context),
    case lists:member(Request#sip_request.method, Allow) of
        true ->
            error_m:return(ok);
        false ->
            % Send "405 Method Not Allowed"
            Response = sip_message:create_response(Request, 405),
            ok = do_send_response(Request, Response, State),
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
            Response = sip_message:create_response(Request, 482),
            ok = do_send_response(Request, Response, State),
            error_m:fail(loop_detected)
    end.

%% Validate message according to the 8.2.2.3
-spec validate_required(#sip_request{}, #state{}) -> error_m:monad(ok).
validate_required(Request, #state{callback = Callback} = State) ->
    Supported = Callback:extensions(Request, State#state.context),
    IsNotSupported = fun (Ext) -> not lists:member(Ext, Supported) end,

    %% FIXME: Ignore for CANCEL requests/ACKs for non-2xx
    Require = sip_message:header_values(require, Request),
    case lists:filter(IsNotSupported, Require) of
        [] ->
            error_m:return(ok);
        Unsupported ->
            % Send "420 Bad Extension"
            Response = sip_message:create_response(Request, 420),
            Response2 = sip_message:append_header('unsupported', Unsupported, Response),
            ok = do_send_response(Request, Response2, State),
            error_m:fail(bad_extension)
    end.

-spec do_send_response(#sip_request{}, #sip_response{}, #state{}) -> ok | {error, Reason :: term()}.
do_send_response(Request, #sip_response{} = Response, #state{callback = Callback} = State) ->

    case do_pre_send(Request, Response) of
        ok ->
            % Append Supported and Allow headers
            Allow = Callback:allowed_methods(Request, State#state.context),
            Extensions = Callback:extensions(Request, State#state.context),
            Response2 = sip_message:append_header(allow, Allow, Response),
            Response3 = sip_message:append_header(supported, Extensions, Response2),

            % send
            {ok, _TxKey} = sip_transaction:send_response(Response3),
            ok;
        {error, Reason} -> {error, Reason}
    end.

do_handle_request(Request, #state{callback = Callback} = State) ->
    Method = Request#sip_request.method,
    case Callback:Method(Request, State#state.context) of
        {noreply, Context} ->
            {ok, State#state{context = Context}};
        {reply, Response, Context} ->
            ok = do_send_response(Request, Response, State),
            {ok, State#state{context = Context}}
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
