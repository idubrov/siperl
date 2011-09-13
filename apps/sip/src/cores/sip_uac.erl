%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC response processing behaviour
%%%
%%% The response handling uses `do' parse transformer from
%%% <a href="https://github.com/rabbitmq/erlando">Erlando</a>. Each step
%%% of the request processing is "valve" function that returns value in
%%% the `pipeline_m' monad. The return is either `{next, State}' (apply
%%% next valve) or `{stop, Result}' (stop message processing and return Result).
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
%%%----------------------------------------------------------------
-module(sip_uac).
-compile({parse_transform, do}).

%% API
-export([create_request/2, send_request/3, handle_info/2]).

%% Response processing
-export([validate_vias/2, process_response/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").


%% @doc Create request outside of the dialog according to the 8.1.1 Generating the Request
%%
%% Creates all required headers of the SIP message: `Via:', `Max-Forwards:',
%% `From:', `To:', `CSeq:', `Call-Id'. Also, adds `Route:' headers
%% if pre-existing route set is configured.
%% @end
-spec create_request(atom() | binary(), #sip_hdr_address{}) -> #sip_message{}.
create_request(Method, ToValue) when
  is_record(ToValue, sip_hdr_address) ->
    % The initial Request-URI of the message SHOULD be set to the value of
    % the URI in the To field.
    RequestURI = ToValue#sip_hdr_address.uri,

    % will be updated later (by transport layer)
    % branch will be added before sending
    Via = {'via', #sip_hdr_via{}},
    MaxForwards = {'max-forwards', 70},
    From = {'from', sip_headers:address(<<"Anonymous">>,
                                        <<"sip:thisis@anonymous.invalid">>,
                                        [{'tag', sip_idgen:generate_tag()}])},
    To = {'to', ToValue},
    % FIXME: for REGISTER requests CSeqs are not arbitrary...
    CSeq = {'cseq', sip_headers:cseq(sip_idgen:generate_cseq(), Method)},
    CallId = {'call-id', sip_idgen:generate_call_id()},

    % configure pre-existing route set
    Routes = [{'route', sip_headers:address(<<>>, sip_uri:parse(RouteBin), [])} || RouteBin <- sip_config:routes()],

    #sip_message{kind = #sip_request{method = Method, uri = RequestURI},
                 headers = [Via, MaxForwards, From, To, CSeq, CallId] ++ Routes}.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% FIXME: Must have Contact: header if request can establish dialog (INVITE)
%% @end
-spec send_request(#sip_message{}, term(), #sip_ua_state{}) -> {ok, #sip_ua_state{}}.
send_request(Request, UserData, State) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,

    % Put Request URI into the target set
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),
    ReqInfo = #req_info{request = Request, user_data = UserData, target_set = TargetSet},

    % Must finish with {noreply, State2}
    {stop, {noreply, State2}} = next_uri(ReqInfo, none, State),
    {ok, State2}.


%% @private
-spec handle_info(term(), #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
%% @doc Process received response by pushing it through our processing pipeline
%% @end
handle_info({response, Response}, State) ->
    do_process_response(Response, State);

%% @doc Process terminated client transactions
%% @end
handle_info({tx, TxKey, {terminated, _Reason}}, State) when is_record(TxKey, sip_tx_client) ->
    % remove transaction from the list
    Dict = dict:erase(TxKey, State#sip_ua_state.requests),
    pipeline_m:stop({noreply, State#sip_ua_state{requests = Dict}});

handle_info(_Info, State) ->
    pipeline_m:next(State).

%% Validate message according to the 8.1.3.3
-spec validate_vias(#sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
validate_vias(Msg, State) ->
    Count = length(sip_message:header_values('via', Msg)),
    case Count of
        1 ->
            pipeline_m:next(State);
        _Other ->
            % discard response, too much/few Via's
            error_logger:warning_report(['message_discarded',
                                         {reason, wrong_vias},
                                         {msg, Msg}]),
            pipeline_m:stop({noreply, State})
    end.

%% @doc Automatic handling of 3xx-6xx responses valve
%% @end
-spec process_response(#req_info{}, #sip_message{}, #sip_ua_state{}) -> pipeline_m:monad(#sip_ua_state{}).
process_response(ReqInfo, #sip_message{kind = #sip_response{status = Status}} = Response, State)
  when Status >= 300, Status =< 399 ->
    ReqInfo2 = collect_redirects(ReqInfo, Response),

    % try next URI, was redirected
    next_uri(ReqInfo2, Response, State);
process_response(ReqInfo, #sip_message{kind = #sip_response{status = Status}} = Response, State)
  when Status =:= 503; Status =:= 408 ->
    % RFC 3263, 4.3. Processing failed responses, try next IP address
    next_destination(ReqInfo, Response, State);
process_response(ReqInfo, #sip_message{kind = #sip_response{status = Status}} = Response, State)
  when Status > 399 ->
    % Try next Contact in target set (if was redirected)
    % otherwise, will process response as is
    next_uri(ReqInfo, Response, State);
process_response(_ReqInfo, _Msg, State) ->
    pipeline_m:next(State).

do_process_response(Response, State) ->
    % fetch request info
    TxKey = sip_transaction:tx_key(client, Response),
    ReqInfo = dict:fetch(TxKey, State#sip_ua_state.requests),

    % remove request info
    Dict = dict:erase(TxKey, State#sip_ua_state.requests),
    State2 = State#sip_ua_state{requests = Dict},


    Mod = State2#sip_ua_state.callback,
    do([pipeline_m ||
        % FIXME: re-send if failed and have more destinations/target URIs.
        S1 <- validate_vias(Response, State2),
        S2 <- process_response(ReqInfo, Response, S1),
        S3 <- Mod:handle_response(ReqInfo#req_info.user_data, Response, S2),
        % TODO: log unhandled requests?
        S3]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

next_uri(ReqInfo, Response, State) ->
    case sip_priority_set:take(ReqInfo#req_info.target_set) of
        false ->
            % No more URIs to try -- continue with last response
            pipeline_m:next(State);
        {value, URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
            Request = ReqInfo#req_info.request,
            Kind = Request#sip_message.kind,
            Request2 = Request#sip_message{kind = Kind#sip_request{uri = URI}},
            ReqInfo2 = ReqInfo#req_info{request = Request2,
                                        destinations = lookup_destinations(Request2),
                                        target_set = TargetSet2},
            next_destination(ReqInfo2, Response, State)
    end.


%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
next_destination(#req_info{destinations = []}, _Response, State) ->
    % no destination IPs to try, continue with last response
    pipeline_m:next(State);
next_destination(#req_info{request = Request, destinations = [Top | Fallback]} = ReqInfo, _Response, State) ->
    % send request to the top destination
    {ok, TxKey} = sip_transaction:start_client_tx(self(), Top, Request),

    % store tx id to {user data, fallback destinations, request} mapping
    ReqInfo2 = ReqInfo#req_info{destinations = Fallback},
    Dict = dict:store(TxKey, ReqInfo2, State#sip_ua_state.requests),

    State2 = State#sip_ua_state{requests = Dict},
    pipeline_m:stop({noreply, State2}).

lookup_destinations(Request) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,
    URI =
        case sip_message:has_header(route, Request) of
            false -> RequestURI;
            true ->
                Route = sip_message:header_top_value(route, Request),
                % See RFC3261, 8.1.2
                % If first element in the route set is strict router,
                % use Request-URI
                case is_strict_router(Route#sip_hdr_address.uri) of
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
    NewTargetSet = sip_message:foldl_headers('contact', CollectFun, ReqInfo#req_info.target_set, Response),
    ReqInfo#req_info{target_set = NewTargetSet, destinations = []}.

% FIXME: Move to sip_uri.
is_strict_router(#sip_uri{params = Params}) ->
    not proplists:get_bool('lr', Params).


%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec ua_test_() -> term().
ua_test_() ->
    [?_assertEqual(true, is_strict_router(sip_uri:parse(<<"sip:p3.middle.com">>))),
     ?_assertEqual(false, is_strict_router(sip_uri:parse(<<"sip:p2.example.com;lr">>)))
    ].
-endif.
