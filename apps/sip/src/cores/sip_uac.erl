%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC core implementation
%%%
%%% Implements OTP `gen_server' behaviour.
%%%
%%% Automatic response handling:
%%% <ul>
%%% <li>If response is redirect (3xx), populate target set with values from
%%% Contact header(s) and send request to next URI from the target set.</li>
%%% <li>If response is 503 (Service Unavailable) or 408 (Request Timeout), try
%%% next IP address (RFC 3263). If no such destinations, try next URI from the
%%% target set. If no URIs in target set, let callback handle the response.</li>
%%% <li>If another failed response is detected, try next URI from target set.
%%% If target set is empty, let callback handle the response.</li>
%%% <li>Otherwise, let UAC callback handle the response.</li>
%%% </ul>
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_uac).
-behaviour(gen_server).
-compile({parse_transform, do}).

%% API
-export([start_link/0, create_request/3, send_request/2, send_request/3, send_request_sync/2, cancel/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type callback() :: fun((reference(), {ok, #sip_response{}}) -> ok).
-type target_set() :: sip_priority_set:priority_set(#sip_uri{}).

-record(request_info,
        {id                :: reference(),          % Unique request identifier
         current_tx        :: #sip_tx_client{},     % Key of active transaction
         cancel_tx         :: #sip_tx_client{},     % `CANCEL' transaction key
         request           :: #sip_request{},       % Original request
         destinations = [] :: [#sip_destination{}], % list of IP addresses to try next
         target_set        :: target_set(),         % URI to visit next (redirects)
         provisional = false :: boolean(),          % If provisional response was received already
         cancel = false    :: boolean(),            % Start `CANCEL' transaction on next provisional
         last_destination  :: #sip_destination{},   % Destination last transaction was sent to
         callback          :: callback()}).         % Callback to invoke on responses

%% Pending requests (that have not received final responses yet)
%% First element is unique request id (that does not change accross retries),
%% second element is current transaction that makes the request and third
%% element is information about the request
-record(state, {requests = [] :: [#request_info{}]}).


-type gen_from() :: {pid(), term()}.

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

%% @doc Create request outside of the dialog according to the 8.1.1 Generating the Request
%%
%% Creates all required headers of the SIP message: `Via:', `Max-Forwards:',
%% `From:', `To:', `CSeq:', `Call-Id'. Also, adds `Route:' headers
%% if pre-existing route set is configured.
%%
%% Clients are free to modify any part of the request according to their needs.
%% @end
-spec create_request(atom() | pid(), sip_name(), #sip_hdr_address{}) -> #sip_request{}.
create_request(UAC, Method, To) when is_record(To, sip_hdr_address) ->
    gen_server:call(UAC, {create_request, Method, To}).

%% @doc Send the request asynchronously. Responses will be provided via
%% `Callback' function calls.
%% <em>Note: callback function will be evaluated on a different process!</em>
%% @end
-spec send_request(atom() | pid(), sip_message(), fun((#sip_response{}) -> ok)) -> {ok, reference()}.
send_request(UAC, Request, Callback) when is_record(Request, sip_request), is_function(Callback, 2) ->
    Ref = make_ref(),
    ok = gen_server:cast(UAC, {send_request, Ref, Request, Callback}),
    {ok, Ref}.

%% @doc Send the request asynchronously. Responses will be provided via
%% `{response, Response}' messages delivered to the caller
%% @end
-spec send_request(atom() | pid(), sip_message()) -> ok.
send_request(UAC, Request) when is_record(Request, sip_request) ->
    Pid = self(),
    send_request(UAC, Request, fun(Ref, {ok, Response}) -> Pid ! {response, Ref, Response} end).

%% @doc Send the request synchronously
%% Returns the final response to the request. Should not be invoked for `INVITE',
%% since all provisional responses will be ignored anyway. Also, `INVITE'
%% processing is usually much longer, so `gen_server' timeout could occur.
%% @end
-spec send_request_sync(atom() | pid(), sip_message()) -> {ok, #sip_response{}} | {error, Reason :: term()}.
send_request_sync(UAC, Request) when is_record(Request, sip_request) ->
    gen_server:call(UAC, {send_request, Request}).

-spec cancel(pid(), reference()) -> ok | {error, no_request}.
%% @doc Cancel the request identified by the reference
%% <em>Note that it is still possible for the client to receive 2xx response
%% on the request that was successfully cancelled. This is due to the inherent
%% race condition present. For example, this could happen if cancel is invoked
%% before UAC have received 2xx response, but after it was sent by the remote side.
%% That means, client should be ready to issue `BYE' when 2xx is received on
%% request it has cancelled.</em>.
%% @end
cancel(UAC, Ref) when is_pid(UAC), is_reference(Ref) ->
    gen_server:call(UAC, {cancel, Ref}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    % FIXME: should register as SIP messages handler to receive additional 2xx responses!
    {ok, #state{}}.

%% @private
-spec handle_call
    ({create_request, sip_name(), #sip_hdr_address{}}, gen_from(), #state{}) ->
        {reply, #sip_request{}, #state{}};
    ({send_request, #sip_request{}}, gen_from(), #state{}) ->
        {noreply, #state{}}.
handle_call({create_request, Method, To}, _From, State) ->
    Request = do_create_request(Method, To),
    {reply, Request, State};

handle_call({send_request, Request}, From, State) ->
    % callback that will ignore all provisional responses
    Callback = fun(_Ref, {ok, #sip_response{status = Status}}) when Status >= 100, Status =< 199 -> ok;
                  (_Ref, Result) -> gen_server:reply(From, Result)
               end,
    {ok, State2} = do_send_request(make_ref(), Request, Callback, State),
    {noreply, State2};

handle_call({cancel, Id}, _From, State) ->
    ReqInfo = lookup_info(Id, State),
    {Reply, State2} = do_cancel(ReqInfo, State),
    {reply, Reply, State2};

handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({send_request, Ref, Request, Callback}, State) ->
    {ok, State2} = do_send_request(Ref, Request, Callback, State),
    {noreply, State2};
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({response, #sip_response{}, #sip_tx_client{method = 'CANCEL'}}, State) ->
    % FIXME: probably, CANCEL transaction should not report to this TU?
    % or: TU should start CANCEL transaction?
    {noreply, State};

handle_info({response, #sip_response{} = Response, #sip_tx_client{} = TxKey}, State) ->
    {ok, State2} = do_response(Response, TxKey, State),
    {noreply, State2};

handle_info({tx, _TxKey, {terminated, _Reason}}, State) ->
    % Just ignore
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

do_create_request(Method, ToAddress) ->
    % The initial Request-URI of the message SHOULD be set to the value of
    % the URI in the To field.
    RequestURI = ToAddress#sip_hdr_address.uri,

    % will be updated later (by transport layer)
    % FIXME: Branch should be generated before sending the reuest!
    Via = {via, #sip_hdr_via{params = [{branch, sip_idgen:generate_branch()}]}},
    MaxForwards = {'max-forwards', 70},
    From = {from, sip_headers:address(<<"Anonymous">>,
                                        <<"sip:thisis@anonymous.invalid">>,
                                        [{'tag', sip_idgen:generate_tag()}])},
    To = {to, ToAddress},
    % FIXME: for REGISTER requests CSeqs are not arbitrary...
    CSeq = {cseq, sip_headers:cseq(sip_idgen:generate_cseq(), Method)},
    CallId = {'call-id', sip_idgen:generate_call_id()},

    % configure pre-existing route set
    Routes = [{'route', sip_headers:address(<<>>, sip_uri:parse(RouteBin), [])} || RouteBin <- sip_config:routes()],

    #sip_request{method = Method,
                 uri = RequestURI,
                 headers = [Via, MaxForwards, From, To, CSeq, CallId | Routes]}.

% Put Request URI into the target set
do_send_request(Id, Request, Callback, State) ->
    ok = sip_message:validate_request(Request),

    RequestURI = Request#sip_request.uri,
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),

    ReqInfo = #request_info{id = Id,
                            request = Request,
                            target_set = TargetSet,
                            callback = Callback},
    % What if `ok' is returned (meaning request was not processed)?
    {error, {processed, State2}} = next_uri(ReqInfo, none, State),
    {ok, State2}.

do_response(Response, TxKey, State) ->
    % search by value (transaction key), it is unique by design
    ReqInfo = lists:keyfind(TxKey, #request_info.current_tx, State#state.requests),

    Result = do([error_m ||
                 S1 <- validate_vias(Response, State),
                 S2 <- handle_provisional_response(ReqInfo, Response, S1),
                 S3 <- handle_redirect_response(ReqInfo, Response, S2),
                 S4 <- handle_failure_response(ReqInfo, Response, S3),
                 S5 <- handle_final_response(ReqInfo, Response, S4),
                 invoke_callback(ReqInfo, Response, S5)]),
    case Result of
        {ok, State2} -> {ok, State2};
        {error, {_Reason, State2}} ->
            % Response was either discarded or is still being
            % processed (following redirect, trying next IP, etc)
            {ok, State2}
    end.

%% Validate message according to the 8.1.3.3
-spec validate_vias(#sip_response{}, #state{}) -> error_m:monad(#state{}).
validate_vias(Msg, State) ->
    Count = length(sip_message:header_values('via', Msg)),
    case Count of
        1 ->
            error_m:return(State);
        _Other ->
            % discard response, too much/few Via's
            sip_log:wrong_vias(Msg),
            error_m:fail({discarded, State})
    end.


-spec handle_provisional_response(#request_info{}, #sip_response{}, #state{}) -> error_m:monad(#state{}).
%% @doc Automatic handling of provisional (1xx) responses
%% @end
handle_provisional_response(ReqInfo, #sip_response{status = Status}, State)
  when Status >= 100, Status =< 199,
       ReqInfo#request_info.cancel,
       ReqInfo#request_info.cancel_tx =:= undefined ->

    % Start `CANCEL' tx, we got provisional response and have not started `CANCEL' tx yet
    ReqInfo2 = ReqInfo#request_info{provisional = true},
    send_cancel(ReqInfo2, State);
handle_provisional_response(ReqInfo, #sip_response{status = Status}, State)
  when Status >= 100, Status =< 199,
       not ReqInfo#request_info.provisional ->
    % We may receive cancellation request later, so update provisional flag
    State2 = store_info(ReqInfo#request_info{provisional = true}, State),
    error_m:return(State2);
handle_provisional_response(_ReqInfo, _Response, State) ->
    error_m:return(State).

-spec handle_redirect_response(#request_info{}, #sip_response{}, #state{}) -> error_m:monad(#state{}).
%% @doc Automatic handling of redirect (3xx) responses
%% @end
handle_redirect_response(ReqInfo, #sip_response{status = Status} = Response, State) when Status >= 300, Status =< 399 ->
    ReqInfo2 = collect_redirects(ReqInfo, Response),
    % try next URI, was redirected
    next_uri(ReqInfo2, Response, State);
handle_redirect_response(_ReqInfo, _Response, State) ->
    error_m:return(State).

-spec handle_failure_response(#request_info{}, #sip_response{}, #state{}) -> error_m:monad(#state{}).
%% @doc Automatic handling of failure (4xx-6xx) responses
%% @end
handle_failure_response(ReqInfo, #sip_response{status = Status} = Response, State) when Status =:= 503; Status =:= 408 ->
    % failed with 408 or 503, try next IP address
    next_destination(ReqInfo, Response, State);
handle_failure_response(ReqInfo, #sip_response{status = Status} = Response, State) when Status >= 400 ->
    % failed, try next URI
    next_uri(ReqInfo, Response, State);
handle_failure_response(_ReqInfo, _Response, State) ->
    error_m:return(State).

-spec handle_final_response(#request_info{}, #sip_response{}, #state{}) -> error_m:monad(#state{}).
%% @doc Remove request information, if final response is received
%% @end
handle_final_response(ReqInfo, #sip_response{status = Status}, State) when Status >= 200 ->
    State2 = delete_info(ReqInfo, State),
    error_m:return(State2);

handle_final_response(_ReqInfo, _Response, State) ->
    error_m:return(State).

%% @doc Invoke callback
%% @end
invoke_callback(ReqInfo, Response, State) when ReqInfo#request_info.cancel, Response#sip_response.status =:= 408->
    % We got 408 Request Timeout and request was cancelled, treat it as 487 Request Terminated
    % (RFC 2543 compliant UAS will not generate such a response)
    Response2 = Response#sip_response{status = 487, reason = sip_message:default_reason(487)},
    invoke_callback(ReqInfo, Response2, State);

invoke_callback(ReqInfo, Response, State) ->
    Id = ReqInfo#request_info.id,
    Callback = ReqInfo#request_info.callback,
    _Ignore = Callback(Id, {ok, Response}),
    error_m:return(State).

next_uri(ReqInfo, Response, State) ->
    case sip_priority_set:take(ReqInfo#request_info.target_set) of
        false ->
            % No more URIs to try, continue processing
            error_m:return(State);
        {value, URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
            Request = ReqInfo#request_info.request,
            Request2 = Request#sip_request{uri = URI},
            ReqInfo2 = ReqInfo#request_info{request = Request2,
                                            destinations = lookup_destinations(Request2),
                                            target_set = TargetSet2},
            next_destination(ReqInfo2, Response, State)
    end.


%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
next_destination(#request_info{destinations = []}, _Response, State) ->
    % no destination IPs to try, continue processing
    error_m:return(State);
next_destination(#request_info{request = Request, destinations = [Top | Fallback]} = ReqInfo, _Response, State) ->
    % Every new client transaction must have its own branch value
    Request2 = sip_message:with_branch(sip_idgen:generate_branch(), Request),
    {ok, TxKey} = sip_transaction:start_client_tx(self(), Top, Request2),

    % Update request information
    ReqInfo2 = ReqInfo#request_info{destinations = Fallback,
                                    last_destination = Top,
                                    current_tx = TxKey},
    State2 = store_info(ReqInfo2, State),

    % stop processing
    error_m:fail({processed, State2}).

lookup_destinations(Request) ->
    RequestURI = Request#sip_request.uri,
    URI =
        case sip_message:has_header(route, Request) of
            false -> RequestURI;
            true ->
                Route = sip_message:header_top_value(route, Request),
                % See RFC3261, 8.1.2
                % If first element in the route set is strict router,
                % use Request-URI
                case sip_uri:is_strict_router(Route#sip_hdr_address.uri) of
                    true -> RequestURI;
                    false -> Route
                end
        end,

    % if the Request-URI specifies a SIPS resource, consider URI to be SIPS as well
    URI2 =
        case RequestURI of
            #sip_uri{scheme = sips} -> URI#sip_uri{scheme = sips};
            _ -> URI
        end,
    sip_resolve:client_resolve(URI2).

collect_redirects(ReqInfo, Response) ->
    % FIXME: handle expires
    CollectFun =
        fun (Contact, TargetSet) ->
                 QValue =
                     case lists:keyfind(q, 1, Contact#sip_hdr_address.params) of
                         false -> 1.0;
                         {_Key, Value} when is_float(Value) -> Value
                     end,
                 sip_priority_set:put(Contact#sip_hdr_address.uri, QValue, TargetSet)
        end,
    % go through Contact: headers and add URIs to our current redirect set
    NewTargetSet = sip_message:foldl_headers(contact, CollectFun, ReqInfo#request_info.target_set, Response),
    ReqInfo#request_info{target_set = NewTargetSet, destinations = []}.


%% @doc Send `CANCEL' transaction, update the state
%% @end
send_cancel(#request_info{} = ReqInfo, State) ->
    % Create CANCEL request, set branch from current transaction
    Branch = ReqInfo#request_info.current_tx#sip_tx_client.branch,
    Cancel = sip_message:create_cancel(ReqInfo#request_info.request),
    Cancel2 = sip_message:with_branch(Branch, Cancel),
    {ok, CancelTxKey} = sip_transaction:start_client_tx(self(), ReqInfo#request_info.last_destination, Cancel2),

    State2 = store_info(ReqInfo#request_info{cancel = true, cancel_tx = CancelTxKey}, State),
    error_m:return(State2).

%% @doc Cancel the request
%% end
do_cancel(false, State) ->
    {{error, no_request}, State};
do_cancel(#request_info{cancel_tx = undefined, provisional = true} = ReqInfo, State) ->
    % start `CANCEL' transaction
    send_cancel(ReqInfo, State);
do_cancel(ReqInfo, State) ->
    % mark as cancelled, wait until provisional is received
    State2 = store_info(ReqInfo#request_info{cancel = true}, State),
    {ok, State2}.

lookup_info(Id, State) ->
    lists:keyfind(Id, #request_info.id, State#state.requests).

store_info(ReqInfo, State) ->
    Requests = lists:keystore(ReqInfo#request_info.id, #request_info.id, State#state.requests, ReqInfo),
    State#state{requests = Requests}.

delete_info(#request_info{id = Id}, State) ->
    Requests = lists:keydelete(Id, #request_info.id, State#state.requests),
    State#state{requests = Requests}.
