%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS core implementation
%%%
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-8">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_ua).

-behaviour(gen_server).

%% API
-export([create_request/3, send_request/2, send_response/1]).

%% Custom behaviour
-export([behaviour_info/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% UAC/UAS callbacks
-export([handle_request/3, handle_response/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(req_info, {request :: #sip_message{},                 % SIP request message
                   destinations = [] :: [#sip_destination{}], % list of IP addresses to try next
                   target_set = sip_priority_set:new(),      % URI to visit next (redirects)
                   user_data}).                               % Custom user data associated with request

-spec behaviour_info(callbacks | term()) -> [{Function :: atom(), Arity :: integer()}].
behaviour_info(callbacks) ->
    [{handle_request, 3}, {handle_response, 3} | gen_server:behaviour_info(callbacks)];
behaviour_info(_) -> undefined.

%% @doc Create request outside of the dialog according to the 8.1.1 Generating the Request
%%
%% Creates all required headers of the SIP message: `Via:', `Max-Forwards:',
%% `From:', `To:', `CSeq:', `Call-Id', `Contact:'. Also, adds `Route:' headers
%% if pre-existing route set is configured.
%% @end
-spec create_request(atom() | binary(), #sip_hdr_address{}, #sip_hdr_address{}) -> #sip_message{}.
create_request(Method, ToValue, ContactValue) when
  is_record(ToValue, sip_hdr_address), is_record(ContactValue, sip_hdr_address) ->
    % The initial Request-URI of the message SHOULD be set to the value of
    % the URI in the To field.
    RequestURI = ToValue#sip_hdr_address.uri,

    % will be updated later (by transport layer)
    % branch will be added before sending
    Via = {'via', #sip_hdr_via{}},
    MaxForwards = {'max-forwards', 70},
    From = {'from', sip_headers:address(<<"Anonymous">>,
                                        <<"sip:thisis@anonymous.invalid">>,
                                        [{tag, sip_idgen:generate_tag()}])},
    To = {'to', ToValue},
    % FIXME: for REGISTER requests CSeqs are not arbitrary...
    CSeq = {'cseq', sip_headers:cseq(sip_idgen:generate_cseq(), Method)},
    CallId = {'call-id', sip_idgen:generate_call_id()},
    Contact = {'contact', ContactValue},

    % configure pre-existing route set
    Routes = [{'route', sip_headers:address(<<>>, sip_uri:parse(RouteBin), [])} || RouteBin <- sip_config:routes()],

    #sip_message{kind = #sip_request{method = Method, uri = RequestURI},
                 headers = [Via, MaxForwards, From, To, CSeq, CallId, Contact] ++ Routes}.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% @end
-spec send_request(#sip_message{}, term()) -> ok.
send_request(Request, UserData) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,

    % Dispatch asynchronously
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),
    ReqInfo = #req_info{request = Request, user_data = UserData, target_set = TargetSet},
    gen_server:cast(self(), {send, ReqInfo}),
    ok.

%% @doc Send the response according to the XXXXXXXXXXX
%% @end
-spec send_response(#sip_message{}) -> {ok, #sip_tx_server{}}.
send_response(Response) ->
    sip_transaction:send_response(Response).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(term()) -> {ok, #sip_ua_state{}}.
init(Module) ->
    {ok, #sip_ua_state{callback = Module}}.

%% @private
-spec handle_call(term(), term(), #sip_ua_state{}) -> any().
handle_call(_Req, _From, State) ->
    {stop, unexpected, State}.

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast({send, ReqInfo}, State) ->
    send_to_next(none, ReqInfo, State);
handle_cast(_Req, State) ->
   {stop, unexpected, State}.

%% @private
-spec handle_info(term(), #sip_ua_state{}) -> any().
handle_info({request, Msg}, #sip_ua_state{callback = Mod, allow = Allow} = State) ->
    Method = Msg#sip_message.kind#sip_request.method,

    % FIXME: stateless behaviour..
    % start new server transaction
    sip_transaction:start_server_tx(self(), Msg),

    case lists:any(fun (V) -> V =:= Method end, Allow) of
        true ->
            % FIXME: rules 8.2.1 and further
            Mod:handle_request(Method, Msg, State);
        false ->
            % Send 'Method Not Allowed'
            Resp = sip_message:create_response(Msg, 405),

            % Add `Allow:' header
            Resp2 = sip_message:append_header('allow', Allow, Resp),

            % Send response
            send_response(Resp2),
            {noreply, State}
    end;

handle_info({response, Msg}, #sip_ua_state{callback = Mod} = State) ->
    TxKey = sip_transaction:tx_key(client, Msg),

    ReqInfo = dict:fetch(TxKey, State#sip_ua_state.requests),
    UserData = ReqInfo#req_info.user_data,

    % RFC 3261 8.1.3.3 Vias
    case count_via(Msg) of
        1 ->
            % handle response from transaction layer
            Mod:handle_response(UserData, Msg, State);
        _Other ->
            % discard response, too much/few Via's
            error_logger:warning_report(['message_discarded',
                                         {reason, wrong_vias},
                                         {msg, Msg}]),
            {noreply, State}
    end;
handle_info({tx, TxKey, {terminated, normal}}, State) ->
    % remove transaction from the list
    Dict = dict:erase(TxKey, State#sip_ua_state.requests),
    {noreply, State#sip_ua_state{requests = Dict}};
handle_info({tx, TxKey, {terminated, Reason}}, State) ->
    ReqInfo = dict:fetch(TxKey, State#sip_ua_state.requests),

    % remove transaction from the list
    Dict = dict:erase(TxKey, State#sip_ua_state.requests),
    State2 = State#sip_ua_state{requests = Dict},

    % transaction failure, let's retry with new transaction, see 8.1.2
    send_to_next(Reason, ReqInfo, State2);
handle_info(_Req, State) ->
    {stop, unexpected, State}.

%% @private
-spec terminate(term(), #sip_ua_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #sip_ua_state{}, term()) -> {ok, #sip_ua_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Default UAC/UAS callbacks

-spec handle_request(binary() | atom(), #sip_message{}, #sip_ua_state{}) -> any().
handle_request(_Method, Msg, State) ->
    % Send 'Server Internal Error' by default
    Resp = sip_message:create_response(Msg, 500),
    send_response(Resp),
    {noreply, State}.


-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> any().
handle_response(_UserData, #sip_message{kind = #sip_response{status = Status}} = Request, State)
  when Status >= 300, Status =< 399 ->
    % handle automatic redirect.
    process_redirect(Request, State);
handle_response(_UserData, _Msg, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
send_to_next(Reason, #req_info{destinations = []} = ReqInfo, State) ->
    % no destination IPs -- resolve next URI from target_set
    case sip_priority_set:take(ReqInfo#req_info.target_set) of
        false ->
            % Handle failed requests (8.1.3.1, RFC 3261)
            Status = error_to_status(Reason),
            Msg = sip_message:create_response(ReqInfo#req_info.request, Status),
            UserData = ReqInfo#req_info.user_data,

            Mod = State#sip_ua_state.callback,
            Mod:handle_response(UserData, Msg, State);
        {value, URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
            Request = ReqInfo#req_info.request,
            Kind = Request#sip_message.kind,
            Request2 = Request#sip_message{kind = Kind#sip_request{uri = URI}},
            Destinations = lookup_destinations(Request2),
            ReqInfo2 = ReqInfo#req_info{request = Request2, destinations = Destinations, target_set = TargetSet2},
            send_to_next(Reason, ReqInfo2, State)
    end;
send_to_next(_Reason, #req_info{request = Request, destinations = [Top | Fallback]} = ReqInfo, State) ->
    % send request to the top destination
    {ok, TxKey} = sip_transaction:start_client_tx(self(), Top, Request),

    % store tx id to {user data, fallback destinations, request} mapping
    ReqInfo2 = ReqInfo#req_info{destinations = Fallback},
    Dict = dict:store(TxKey, ReqInfo2, State#sip_ua_state.requests),
    {noreply, State#sip_ua_state{requests = Dict}}.

%% @doc Automatic handling of 3xx responses (redirects)
%% @end
-spec process_redirect(#sip_message{}, #sip_ua_state{}) -> any().
process_redirect(Msg, State) ->
    TxKey = sip_transaction:tx_key(client, Msg),

    % collect new URIs
    ReqInfo = dict:fetch(TxKey, State#sip_ua_state.requests),
    % FIXME: handle expires
    CollectFun =
        fun (Contact, TargetSet) ->
                 QValue = sip_headers:qvalue(Contact),
                 sip_priority_set:put(Contact#sip_hdr_address.uri, QValue, TargetSet)
        end,
    % go through Contact: headers and add URIs to our current redirect set
    NewTargetSet = sip_message:foldl_headers('contact', CollectFun, ReqInfo#req_info.target_set, Msg),
    ReqInfo2 = ReqInfo#req_info{target_set = NewTargetSet, destinations = []},
    Transactions2 = dict:store(TxKey, ReqInfo2, State#sip_ua_state.requests),

    % try next destination, was redirected
    send_to_next(no_more_destinations, ReqInfo2, State#sip_ua_state{requests = Transactions2}).

count_via(Msg) ->
    sip_message:foldl_headers('via', fun (_Value, Acc) -> Acc + 1 end, 0, Msg).

error_to_status({timeout, _Timer}) -> 408;
error_to_status({econnrefused, _}) -> 503;
error_to_status(no_more_destinations) -> 503;
error_to_status(_Reason) -> 500.

lookup_destinations(Request) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,
    URI =
        case sip_message:top_header('route', Request) of
            {error, not_found} ->
                RequestURI;
            {ok, #sip_hdr_address{uri = Route}} ->
                % See RFC3261, 8.1.2
                % If first element in the route set is strict router,
                % use Request-URI
                case is_strict_router(Route) of
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

is_strict_router(#sip_uri{params = Params}) ->
    not proplists:get_bool('lr', Params).

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec ua_test_() -> term().
ua_test_() ->
    [?_assertEqual(true, is_strict_router(sip_uri:parse(<<"sip:p3.middle.com">>))),
     ?_assertEqual(false, is_strict_router(sip_uri:parse(<<"sip:p2.example.com;lr">>)))
    ].
-endif.
