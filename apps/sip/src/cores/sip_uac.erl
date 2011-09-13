%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC response processing behaviour
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
-export([new/1, create_request/3, send_request/3, process_response/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(req_info, {request :: #sip_request{},                 % SIP request message
                   destinations = [] :: [#sip_destination{}], % list of IP addresses to try next
                   target_set = sip_priority_set:new(),       % URI to visit next (redirects)
                   user_data}).                               % Custom user data associated with request

-record(uac, {callback :: module()}).

-spec new(module()) -> #uac{}.
new(Module) ->
    #uac{callback = Module}.

%% @doc Create request outside of the dialog according to the 8.1.1 Generating the Request
%%
%% Creates all required headers of the SIP message: `Via:', `Max-Forwards:',
%% `From:', `To:', `CSeq:', `Call-Id'. Also, adds `Route:' headers
%% if pre-existing route set is configured.
%% @end
-spec create_request(#uac{}, sip_name(), #sip_hdr_address{}) -> sip_message().
create_request(UAC, Method, ToValue) when
  is_record(UAC, uac), is_record(ToValue, sip_hdr_address) ->
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

    #sip_request{method = Method,
                 uri = RequestURI,
                 headers = [Via, MaxForwards, From, To, CSeq, CallId] ++ Routes}.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% FIXME: Must have Contact: header if request can establish dialog (INVITE)
%% @end
-spec send_request(#uac{}, sip_message(), term()) -> ok | {error, Reason :: term()}.
send_request(UAC, Request, UserData) when is_record(UAC, uac) ->
    RequestURI = Request#sip_request.uri,

    % Put Request URI into the target set
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),
    ReqInfo = #req_info{request = Request, user_data = UserData, target_set = TargetSet},

    case next_uri(ReqInfo, none) of
        {error, _Reason} -> ok; % processing terminated
        ok -> {error, no_destinations}
    end.


-spec process_response(#uac{}, sip_message(), #req_info{}) -> {ok, UserData :: term()} | {error, Reason :: term()}.
%% @doc Process received response by pushing it through our processing pipeline
%% @end
process_response(UAC, Response, ReqInfo) when is_record(UAC, uac) ->
    do([error_m ||
        validate_vias(Response),
        handle_response(ReqInfo, Response),
        return(ReqInfo#req_info.user_data)]).

%% Internal functions

%% Validate message according to the 8.1.3.3
-spec validate_vias(sip_message()) -> error_m:monad(ok).
validate_vias(Msg) ->
    Count = length(sip_message:header_values('via', Msg)),
    case Count of
        1 ->
            error_m:return(ok);
        _Other ->
            % discard response, too much/few Via's
            error_logger:warning_report(['message_discarded',
                                         {reason, wrong_vias},
                                         {msg, Msg}]),
            error_m:fail(discarded)
    end.

%% @doc Automatic handling of 3xx-6xx responses valve
%% @end
-spec handle_response(#req_info{}, sip_message()) -> error_m:monad(ok).
handle_response(ReqInfo, #sip_response{status = Status} = Response)
  when Status >= 300, Status =< 399 ->
    ReqInfo2 = collect_redirects(ReqInfo, Response),

    % try next URI, was redirected
    next_uri(ReqInfo2, Response);
handle_response(ReqInfo, #sip_response{status = Status} = Response) when Status =:= 503; Status =:= 408 ->
    % failed with 408 or 503, try next IP address
    next_destination(ReqInfo, Response);
handle_response(ReqInfo, #sip_response{status = Status} = Response) when Status > 399 ->
    % failed, try next URI
    next_uri(ReqInfo, Response);
handle_response(_ReqInfo, _Msg) ->
    error_m:return(ok).

next_uri(ReqInfo, Response) ->
    case sip_priority_set:take(ReqInfo#req_info.target_set) of
        false ->
            % No more URIs to try, continue processing
            error_m:return(ok);
        {value, URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
            Request = ReqInfo#req_info.request,
            Request2 = Request#sip_request{uri = URI},
            ReqInfo2 = ReqInfo#req_info{request = Request2,
                                        destinations = lookup_destinations(Request2),
                                        target_set = TargetSet2},
            next_destination(ReqInfo2, Response)
    end.


%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
next_destination(#req_info{destinations = []}, _Response) ->
    % no destination IPs to try, continue processing
    error_m:return(ok);
next_destination(#req_info{request = Request, destinations = [Top | Fallback]} = ReqInfo, _Response) ->
    % send request to the top destination, with new request info
    ReqInfo2 = ReqInfo#req_info{destinations = Fallback},
    sip_transaction:start_client_tx(self(), Top, Request, ReqInfo2),
    % stop processing
    error_m:fail(processed).

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
