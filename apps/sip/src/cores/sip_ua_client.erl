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
%%% <li>If another failed response is detected and target set is not empty,
%%% try next URI from target set</li>
%%% <li>Otherwise, let UAC callback handle the response.</li>
%%% </ul>
%%%
%%% <em>This module makes use of process dictionary to preserve information about
%%% pending requests.</em>
%%%
%%% FIXME: Link somehow to transactions. If pending transaction will die, the request
%%% will stay forever in the list! #14
%%% FIXME: Following redirects should be optional.
%%%
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua_client).
-compile({parse_transform, do}).

%% Internal API
-export([init/1, create_request/2, create_ack/1, send_request/1, cancel_request/1, handle_response/4, handle_down/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% Process dictionary key for requests
-define(REQUESTS, sip_ua_client_requests).

%% Set of URIs to try for given request
-type target_set() :: sip_priority_set:priority_set(#sip_uri{}).

%% Pending requests (that have not received final responses yet)
%% First element is unique request id (that does not change accross retries),
%% second element is current transaction that makes the request and third
%% element is information about the request
-record(request_info,
        {id                  :: reference(),          % Unique request identifier
         current_branch      :: binary(),             % Branch for the current transaction
         tx_pid              :: pid(),                % Key of currenty transaction, serving the request
         tx_monitor          :: reference(),          % Transaction monitor reference
         request             :: #sip_request{},       % Original request
         destinations = []   :: [#sip_destination{}], % list of IP addresses to try next
         target_set          :: target_set(),         % URI to visit next (redirects)
         provisional = false :: boolean(),            % If provisional response was received already
         cancel = false      :: boolean(),            % Start `CANCEL' transaction on next provisional
         cancelled = false   :: boolean(),            % If `CANCEL' transaction is already sent
         last_destination    :: #sip_destination{}}). % Destination last transaction was sent to

-type state() :: term().     % Callback module state

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

-spec init(list()) -> ok.
init(_Opts) ->
    erlang:put(?REQUESTS, []),
    ok.

%% FIXME: should be possible to return error
-spec create_request(sip_name(), #sip_hdr_address{} | #sip_dialog_id{}) -> #sip_request{}.
%% @doc Create request outside of the dialog
%% @end
create_request(Method, #sip_hdr_address{} = ToAddress) ->
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
                 headers = [Via, MaxForwards, From, To, CSeq, CallId | Routes]};
create_request(Method, DialogId) when is_record(DialogId, sip_dialog_id) ->
    {ok, Request} = sip_dialog:create_request(Method, DialogId),
    Request.

-spec create_ack(#sip_response{}) -> #sip_request{}.
create_ack(#sip_response{status = 200} = Response) ->
    DialogId = sip_dialog:dialog_id(uac, Response),
    ACK = create_request('ACK', DialogId),
    CSeq = sip_message:header_top_value(cseq, Response),
    sip_message:replace_top_header(cseq, CSeq#sip_hdr_cseq{method = 'ACK'}, ACK).

-spec send_request(#sip_request{}) -> {ok, reference()} | {error, no_destinations}.
send_request(Request) ->
    RequestId = make_ref(),
    Result = do([error_m ||
                 sip_message:validate_request(Request),
                 sip_ua_session:process_request(uac, Request),
                 next_uri(RequestId, Request)]),
    case Result of
        {error, processed} -> {ok, RequestId};
        {error, Reason} -> {error, Reason};
        ok -> {error, no_destinations} % FIXME: Report to callback!
    end.

-spec cancel_request(reference()) -> ok | {error, no_request}.
cancel_request(Id) ->
    ReqInfo = lookup_by_id(Id),
    do_cancel(ReqInfo).

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

%% @private
-spec handle_response(#sip_response{}, pid(), module(), state()) -> {noreply, state()}.
handle_response(#sip_response{} = Response, TxPid, Callback, State) ->
    #request_info{} = ReqInfo = lookup_by_tx(TxPid),

    % If we got 408 Request Timeout and request was cancelled, treat it as 487 Request Terminated
    % (RFC 2543 compliant UAS will not generate such a response), see 9.1 RFC3261
    Response2 =
        case (ReqInfo#request_info.cancel andalso Response#sip_response.status =:= 408) of
            true ->
                Response#sip_response{status = 487, reason = sip_message:default_reason(487)};
            false ->
                Response
        end,

    % FIXME: Should we handle 2xx with offer here? Probably, sip_ua_client should invoke
    % Callback:answer(Offer) and send ACK automatically?
    Result = do([error_m ||
                 validate_vias(Response2),
                 handle_provisional_response(ReqInfo, Response2),
                 handle_redirect_response(ReqInfo, Response2),
                 handle_failure_response(ReqInfo, Response2),
                 handle_dialog_response(ReqInfo, Response2),
                 sip_ua_session:process_response(uac, ReqInfo#request_info.request, Response2),
                 handle_final_response(ReqInfo, Response2)]),
    % FIXME: 12.2.1.2

    case Result of
        ok ->
            invoke_callback(ReqInfo, Response2, Callback, State);
        {error, _Reason} ->
            % Response was either discarded or is still being
            % processed (following redirect, trying next IP, etc)
            {noreply, State}
    end.

%% @doc Handle transaction termination
%% When transaction is terminated, kill the request and destroy all early dialogs.
%% Note that this function is also used to implement 13.2.2.4 behavior. When 2xx
%% response is received, we rely on RFC 6026 transaction behavior to have the
%% Timer M of 64*T1. When timer fires, we get 'DOWN' event, which we process here.
%% @end
-spec handle_down(reference(), pid(), term()) -> boolean().
handle_down(_Ref, TxPid, _Info) ->
    case lookup_by_tx(TxPid) of
        false -> false;
        #request_info{id = Id, request = Request} ->
            % FIXME: processing non-'normal' terminations!
            % consider request terminated
            ok = destroy_early_dialogs(Request),
            ok = delete(Id),
            true
    end.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% Validate message according to the 8.1.3.3
-spec validate_vias(#sip_response{}) -> error_m:monad(ok).
validate_vias(Msg) ->
    Count = length(sip_message:header_values('via', Msg)),
    case Count of
        1 ->
            error_m:return(ok);
        _Other ->
            % discard response, too much/few Via's
            sip_log:wrong_vias(Msg),
            error_m:fail(discarded)
    end.

-spec handle_provisional_response(#request_info{}, #sip_response{}) -> error_m:monad(ok).
%% @doc Automatic handling of provisional (1xx) responses
%% @end
handle_provisional_response(ReqInfo, #sip_response{status = Status})
  when Status >= 100, Status =< 199,
       ReqInfo#request_info.cancel, not ReqInfo#request_info.cancelled ->

    % Start `CANCEL' tx, we got provisional response and have not started `CANCEL' tx yet
    ReqInfo2 = ReqInfo#request_info{provisional = true},
    send_cancel(ReqInfo2);
handle_provisional_response(ReqInfo, #sip_response{status = Status})
  when Status >= 100, Status =< 199,
       not ReqInfo#request_info.provisional ->
    % We may receive cancellation request later, so update provisional flag
    ok = store(ReqInfo#request_info{provisional = true}),
    error_m:return(ok);
handle_provisional_response(_ReqInfo, _Response) ->
    error_m:return(ok).

-spec handle_redirect_response(#request_info{}, #sip_response{}) -> error_m:monad(ok).
%% @doc Automatic handling of redirect (3xx) responses
%% @end
handle_redirect_response(ReqInfo, #sip_response{status = Status} = Response) when Status >= 300, Status =< 399 ->
    ReqInfo2 = collect_redirects(ReqInfo, Response),
    % try next URI, was redirected
    next_uri(ReqInfo2);
handle_redirect_response(_ReqInfo, _Response) ->
    error_m:return(ok).

-spec handle_dialog_response(#request_info{}, #sip_response{}) -> error_m:monad(ok).
%% @doc Handle dialog creation/termination
%% Create dialog, if success (2xx) response is received and request is not within a dialog.
%% Terminate dialog, if 2xx, 481 or 408 response is received and request was `BYE'
%% @end
handle_dialog_response(#request_info{request = #sip_request{method = 'BYE'}}, #sip_response{status = Status} = Response)
  when (Status >= 200 andalso Status =< 299); Status =:= 481; Status =:= 408 ->

    DialogId = sip_dialog:dialog_id(uac, Response),
    ok = sip_dialog:destroy_invite_usage(DialogId),
    error_m:return(ok);

handle_dialog_response(#request_info{request = Request}, #sip_response{status = Status} = Response)
  when Status >= 200, Status =< 299, Request#sip_request.method =:= 'INVITE' ->
    case sip_message:is_within_dialog(Request) of
        false ->
            % FIXME: non-100 provisional response should create early dialog.
            {ok, _DialogId} = sip_dialog:create_invite_usage(uac, Request, Response),

            % Analyze initial request since we now have dialog
            sip_ua_session:process_invite(uac, Request, Response),
            error_m:return(ok);
        true ->
            % Response to re-INVITE
            error_m:return(ok)
    end;

handle_dialog_response(_ReqInfo, _Response) ->
    error_m:return(ok).


-spec handle_failure_response(#request_info{}, #sip_response{}) -> error_m:monad(ok).
%% @doc Automatic handling of failure (4xx-6xx) responses
%% @end
handle_failure_response(ReqInfo, #sip_response{status = Status}) when Status =:= 503; Status =:= 408 ->
    % failed with 408 or 503, try next IP address
    next_destination(ReqInfo);
handle_failure_response(ReqInfo, #sip_response{status = Status}) when Status >= 400 ->
    % failed, try next URI
    next_uri(ReqInfo);
handle_failure_response(_ReqInfo, _Response) ->
    error_m:return(ok).

%% @doc Handle final responses
%% If non 2xx final response is received, request is immediately deleted
%% If first 2xx final response is received, timer is started
%% All subsequent 2xx final responses are ignored
%% @end
-spec handle_final_response(#request_info{}, #sip_response{}) -> error_m:monad(ok).
handle_final_response(#request_info{request = #sip_request{method = 'INVITE'}},
                      #sip_response{status = Status}) when Status >= 200, Status =< 299 ->
    % we rely on RFC 6026 behavior of transaction layer to terminate because of Timer M
    % continue monitoring the transaction
    error_m:return(ok);
handle_final_response(ReqInfo, #sip_response{status = Status}) when Status >= 200 ->
    % either non-INVITE or non-2xx INVITE
    ok = destroy_early_dialogs(ReqInfo#request_info.request),
    ok = tx_demonitor(ReqInfo),
    ok = delete(ReqInfo#request_info.id),
    error_m:return(ok);
handle_final_response(_ReqInfo, _Response) ->
    error_m:return(ok).

%% @doc Invoke callback
%% @end
invoke_callback(ReqInfo, Response, Callback, State)
  when ReqInfo#request_info.cancel, Response#sip_response.status =:= 408 ->
    % We got 408 Request Timeout and request was cancelled, treat it as 487 Request Terminated
    % (RFC 2543 compliant UAS will not generate such a response)
    Response2 = Response#sip_response{status = 487, reason = sip_message:default_reason(487)},
    invoke_callback(ReqInfo, Response2, Callback, State);

invoke_callback(ReqInfo, Response, Callback, State) ->
    Callback:handle_response(ReqInfo#request_info.request, Response, ReqInfo#request_info.id, State).

next_uri(ReqInfo) ->
    case sip_priority_set:take(ReqInfo#request_info.target_set) of
        false ->
            % No more URIs to try, continue processing
            error_m:return(ok);
        {value, URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
            Request = ReqInfo#request_info.request,
            Request2 = Request#sip_request{uri = URI},
            Destinations = sip_resolve:request_destinations(Request2),
            ReqInfo2 = ReqInfo#request_info{request = Request2,
                                            destinations = Destinations,
                                            target_set = TargetSet2},
            next_destination(ReqInfo2)
    end.

next_uri(RequestId, Request) ->
    RequestURI = Request#sip_request.uri,
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),
    ReqInfo = #request_info{id = RequestId,
                            request = Request,
                            target_set = TargetSet},
    next_uri(ReqInfo).

%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
next_destination(#request_info{destinations = []}) ->
    % no destination IPs to try, continue processing
    error_m:return(ok);

%% @doc Send 'ACK' to the transport layer directly
%% @end
next_destination(#request_info{request = Request, destinations = [Top | _Fallback]})
  when Request#sip_request.method =:= 'ACK' ->

    % FIXME: Since transport layer returns errors immediately, send to fallback destinations
    % if error is returned...
    % FIXME: How UDP errors are to be handled? Maybe, ICMP support on transport layer, track unreachable ports?
    % FIXME: ACK should be on 2xx only...
    % The ACK for a 2xx response to an INVITE request is a separate transaction.
    Branch = sip_idgen:generate_branch(),
    Request2 = sip_message:with_branch(Branch, Request),
    ok = sip_transport:send_request(Top, Request2, []), % FIXME: options..

    % stop processing
    error_m:fail(processed);

%% @doc Send requests other than 'ACK'
%% @end
next_destination(#request_info{request = Request, destinations = [Top | Fallback]} = ReqInfo) ->
    % Every new client transaction must have its own branch value
    Branch = sip_idgen:generate_branch(),
    Request2 = sip_message:with_branch(Branch, Request),

    ok = tx_demonitor(ReqInfo),
    {ok, TxPid} = sip_transaction:start_client_tx(Top, Request2, []),
    % Actually, we should never receive 'DOWN' message since transaction should
    % send us final response in any case
    MonitorRef = erlang:monitor(process, TxPid),

    %% FIXME: should cancel the sesssion
%%     ok = case Request#sip_request.method of
%%              'BYE' ->
%%                 % Once BYE is passed to client transaction, consider session terminated
%%                 DialogId = sip_dialog:dialog_id(uac, Request),
%%                 ok = sip_dialog:destroy_invite_usage(DialogId);
%%              _Other ->
%%                  ok
%%          end,

    % Start expiration timer if Expires: is present
    ok = start_expiration_timer(ReqInfo),

    % Update request information
    ReqInfo2 = ReqInfo#request_info{destinations = Fallback,
                                    last_destination = Top,
                                    current_branch = Branch,
                                    tx_pid = TxPid,
                                    tx_monitor = MonitorRef},
    ok = store(ReqInfo2),

    % stop processing
    error_m:fail(processed).

start_expiration_timer(#request_info{request = Request} = ReqInfo) ->
    case sip_message:has_header(expires, Request) andalso sip_message:is_dialog_establishing(Request) of
        true ->
            Expires = sip_message:header_top_value(expires, Request),
            % ignore the timer reference, we do not cancel it. cancel_request will return {error, no_request}
            % if it is too late to cancel the request (final response was received)
            _Timer = erlang:send_after(Expires * 1000, self(), {cancel_request, ReqInfo#request_info.id}),
            ok;
        false ->
            ok
    end.

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

%% @doc Cancel the request
%% end
do_cancel(false) ->
    {error, no_request};
do_cancel(#request_info{cancelled = false, provisional = true} = ReqInfo) ->
    % start `CANCEL' transaction
    send_cancel(ReqInfo);
do_cancel(ReqInfo) ->
    % mark as cancelled, wait until provisional is received
    ok = store(ReqInfo#request_info{cancel = true}).

%% @doc Send `CANCEL' transaction, update the state
%% @end
send_cancel(#request_info{} = ReqInfo) ->
    % Create CANCEL request, set branch from current transaction,
    % start transaction without TU (fire-and-forget)
    Branch = ReqInfo#request_info.current_branch,
    Cancel = sip_message:create_cancel(ReqInfo#request_info.request),
    Cancel2 = sip_message:with_branch(Branch, Cancel),

    Destination = ReqInfo#request_info.last_destination,
    {ok, _CancelTxPid} = sip_transaction:start_client_tx(Destination, Cancel2, [{tu, none}]),

    ok = store(ReqInfo#request_info{cancel = true, cancelled = true}).


tx_demonitor(#request_info{tx_monitor = undefined}) -> ok;
tx_demonitor(#request_info{tx_monitor = MonitorRef}) ->
    true = erlang:demonitor(MonitorRef, [flush]),
    ok.

destroy_early_dialogs(#sip_request{method = 'INVITE'} = _Request) ->
    ok;
destroy_early_dialogs(_Request) ->
    ok.

%%-----------------------------------------------------------------
%% State management (functions that use process dictionary)
%%-----------------------------------------------------------------

lookup_by_id(Id) ->
    Requests = erlang:get(?REQUESTS),
    lists:keyfind(Id, #request_info.id, Requests).

lookup_by_tx(TxPid) ->
    Requests = erlang:get(?REQUESTS),
    lists:keyfind(TxPid, #request_info.tx_pid, Requests).


store(ReqInfo) ->
    Requests = erlang:get(?REQUESTS),
    Requests2 = lists:keystore(ReqInfo#request_info.id, #request_info.id, Requests, ReqInfo),
    erlang:put(?REQUESTS, Requests2),
    ok.

delete(Id) ->
    Requests = erlang:get(?REQUESTS),
    Requests2 = lists:keydelete(Id, #request_info.id, Requests),
    erlang:put(?REQUESTS, Requests2),
    ok.
