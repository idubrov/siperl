%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
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
-export([handle_request/3, handle_response/3, is_applicable/1]).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

-record(req_info, {request :: #sip_message{},                % SIP request message
                   destinations = [] :: [inet:ip_address()], % list of IP addresses to try next
                   target_set = orddict:new(),               % URI to visit next (redirects)
                   user_data}).                              % Custom user data associated with request

-spec behaviour_info(callbacks | term()) -> [{Function :: atom(), Arity :: integer()}].
behaviour_info(callbacks) ->
    [{handle_request, 3}, {handle_response, 3}, {is_applicable, 1} | gen_server:behaviour_info(callbacks)];
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
    Routes = [{'route', sip_headers:route(Route, [])} || Route <- sip_config:routes()],

    #sip_message{kind = #sip_request{method = Method, uri = RequestURI},
                 headers = [Via, MaxForwards, From, To, CSeq, CallId, Contact] ++ Routes}.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% @end
-spec send_request(#sip_message{}, term()) -> ok.
send_request(Request, UserData) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,

    % Dispatch asynchronously
    ReqInfo = #req_info{request = Request, user_data = UserData, target_set = [{RequestURI, false}]},
    gen_server:cast(self(), {send, ReqInfo}),
    ok.

%% @doc Send the response according to the XXXXXXXXXXX
%% @end
-spec send_response(#sip_message{}) -> {ok, binary()}.
send_response(Response) ->
    sip_transaction:send_response(Response).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(term()) -> {ok, #sip_ua_state{}}.
init(Module) ->
    IsApplicable = fun (Msg) -> Module:is_applicable(Msg) end,
    sip_cores:register_core(#sip_core_info{is_applicable = IsApplicable}),
    {ok, #sip_ua_state{mod = Module}}.

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
handle_info({request, Msg}, #sip_ua_state{mod = Mod} = State) ->
    % FIXME: rules 8.2.1 and further
    % start new server transaction
    sip_transaction:start_server_tx(self(), Msg),

    Mod:handle_request(Msg#sip_message.kind#sip_request.method, Msg, State);
handle_info({response, Msg}, #sip_ua_state{mod = Mod} = State) ->
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
    % Send 'Method Not Allowed' by default
    Resp = sip_message:create_response(Msg, 405),
    send_response(Resp),
    {noreply, State}.


-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> any().
handle_response(_UserData, #sip_message{kind = #sip_response{status = Status}} = Request, State)
  when Status >= 300, Status =< 399 ->
    % handle automatic redirect.
    process_redirect(Request, State);
handle_response(_UserData, _Msg, State) ->
    {noreply, State}.

-spec is_applicable(#sip_message{}) -> boolean().
is_applicable(_Msg) -> false.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
send_to_next(Reason, #req_info{destinations = [], request = Request} = ReqInfo, #sip_ua_state{mod = Mod} = State) ->
    % no destination IPs -- resolve next URI from target_set
    case select_target(ReqInfo#req_info.target_set) of
        none ->
            % Handle failed requests (8.1.3.1, RFC 3261)
            Status = error_to_status(Reason),
            Msg = sip_message:create_response(ReqInfo#req_info.request, Status),
            UserData = ReqInfo#req_info.user_data,
            Mod:handle_response(UserData, Msg, State);
        {URI, TargetSet2} ->
            % Update Request-URI
            % FIXME: Update headers as well!
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
    % FIXME: handle expires & q
    % add {URI, false} (only if not present already)
    CollectFun =
        fun (Contact, Targets) ->
                 orddict:update(Contact#sip_hdr_address.uri, fun (V) -> V end, false, Targets)
        end,
    % go through Contact: headers and add URIs to our current redirect set
    NewTargetSet = sip_message:foldl_headers('contact', CollectFun, ReqInfo#req_info.target_set, Msg),
    ReqInfo2 = ReqInfo#req_info{target_set = NewTargetSet, destinations = []},
    Transactions2 = dict:store(TxKey, ReqInfo2, State#sip_ua_state.requests),

    % try next destination, was redirected
    send_to_next(no_more_destinations, ReqInfo2, State#sip_ua_state{requests = Transactions2}).

count_via(Msg) ->
    Fun = fun ({'via', List}, Acc) when is_list(List) -> Acc + length(List);
              ({'via', _Value}, Acc) -> Acc + 1;
              (_Header, Acc) -> Acc
          end,
    lists:foldl(Fun, 0, Msg#sip_message.headers).

error_to_status({timeout, _Timer}) -> 408;
error_to_status({econnrefused, _}) -> 503;
error_to_status(no_more_destinations) -> 503;
error_to_status(_Reason) -> 500.

select_target([]) -> none;
select_target([{URI, false} | Rest]) ->
    % mark destination as visited
    {URI, [{URI, true} | Rest]};
select_target([Top | Rest]) ->
    % try to lookup in the rest URIs
    case select_target(Rest) of
        none -> none;
        {URI, Rest2} -> {URI, [Top | Rest2]}
    end.

lookup_destinations(Request) ->
    RequestURI = Request#sip_message.kind#sip_request.uri,
    URI =
        case sip_message:top_header('route', Request) of
            {error, not_found} ->
                RequestURI;
            {ok, #sip_hdr_address{uri = Route}} ->
                case sip_headers:is_strict_router(Route) of
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
