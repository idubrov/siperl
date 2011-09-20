%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Internal UAC core implementation. Clients should use `sip_ua' API.
%%%
%%% Used as a "mixin" in the `sip_ua' gen_server implementation.
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
-module(sip_ua_client).
-compile({parse_transform, do}).

%% Internal API
-export([init/1, create_request/2, send_request/3, cancel_request/2, handle_response/3]).

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
-record(uac_state, {requests = [] :: [#request_info{}]}).

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

-spec init(module()) -> {ok, #uac_state{}}.
init(_Callback) ->
    {ok, #uac_state{}}.

-spec create_request(sip_name(), #sip_hdr_address{}) -> #sip_request{}.
create_request(Method, ToAddress) ->
    % The initial Request-URI of the message SHOULD be set to the value of
    % the URI in the To field.
    RequestURI = ToAddress#sip_hdr_address.uri,

    % will be updated later (by transport layer)
    Via = {via, #sip_hdr_via{}},
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
-spec send_request(#sip_request{}, callback(), #uac_state{}) -> {{ok, reference()}, #uac_state{}} | {{error, no_destinations}, #uac_state{}}.
send_request(Request, Callback, State) ->
    Id = make_ref(),
    ok = sip_message:validate_request(Request),

    RequestURI = Request#sip_request.uri,
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),

    ReqInfo = #request_info{id = Id,
                            request = Request,
                            target_set = TargetSet,
                            callback = Callback},
    case next_uri(ReqInfo, none, State) of
        {error, {processed, State2}} -> {{ok, Id}, State2};
        {ok, State2} -> {{error, no_destinations}, State2}
    end.

-spec cancel_request(reference(), #uac_state{}) -> {ok, #uac_state{}} | {{error, no_request}, #uac_state{}}.
cancel_request(Id, State) ->
    ReqInfo = lookup_info(Id, State),
    do_cancel(ReqInfo, State).

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

%% @private
-spec handle_response(#sip_response{}, #sip_tx_client{}, #uac_state{}) -> {noreply, #uac_state{}}.
handle_response(#sip_response{}, #sip_tx_client{method = 'CANCEL'}, State) ->
    % ignore response for 'CANCEL' request
    {ok, State};

handle_response(#sip_response{} = Response, #sip_tx_client{} = TxKey, State) ->
    % search by value (transaction key), it is unique by design
    ReqInfo = lists:keyfind(TxKey, #request_info.current_tx, State#uac_state.requests),

    Result = do([error_m ||
                 S1 <- validate_vias(Response, State),
                 S2 <- handle_provisional_response(ReqInfo, Response, S1),
                 S3 <- handle_redirect_response(ReqInfo, Response, S2),
                 S4 <- handle_failure_response(ReqInfo, Response, S3),
                 S5 <- handle_success_response(ReqInfo, Response, S4),
                 S6 <- handle_final_response(ReqInfo, Response, S5),
                 invoke_callback(ReqInfo, Response, S6)]),
    case Result of
        {ok, State2} -> {ok, State2};
        {error, {_Reason, State2}} ->
            % Response was either discarded or is still being
            % processed (following redirect, trying next IP, etc)
            {ok, State2}
    end.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% Validate message according to the 8.1.3.3
-spec validate_vias(#sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
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

-spec handle_provisional_response(#request_info{}, #sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
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

-spec handle_redirect_response(#request_info{}, #sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
%% @doc Automatic handling of redirect (3xx) responses
%% @end
handle_redirect_response(ReqInfo, #sip_response{status = Status} = Response, State) when Status >= 300, Status =< 399 ->
    ReqInfo2 = collect_redirects(ReqInfo, Response),
    % try next URI, was redirected
    next_uri(ReqInfo2, Response, State);
handle_redirect_response(_ReqInfo, _Response, State) ->
    error_m:return(State).

-spec handle_success_response(#request_info{}, #sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
%% @doc Create dialog state, if success (2xx) response is received and request is dialog establishing
%% @end
handle_success_response(ReqInfo, #sip_response{status = Status} = Response, State) when Status >= 200, Status =< 299 ->
    Request = ReqInfo#request_info.request,

    case sip_message:is_dialog_establishing(Request) of
        true ->
            ok = sip_dialog:create_dialog(uac, Request, Response),
            error_m:return(State);
        false ->
            error_m:return(State)
    end;

handle_success_response(_ReqInfo, _Response, State) ->
    error_m:return(State).


-spec handle_failure_response(#request_info{}, #sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
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

-spec handle_final_response(#request_info{}, #sip_response{}, #uac_state{}) -> error_m:monad(#uac_state{}).
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
    lists:keyfind(Id, #request_info.id, State#uac_state.requests).

store_info(ReqInfo, State) ->
    Requests = lists:keystore(ReqInfo#request_info.id, #request_info.id, State#uac_state.requests, ReqInfo),
    State#uac_state{requests = Requests}.

delete_info(#request_info{id = Id}, State) ->
    Requests = lists:keydelete(Id, #request_info.id, State#uac_state.requests),
    State#uac_state{requests = Requests}.
