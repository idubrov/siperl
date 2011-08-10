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
-export([start_link/3]).
-export([create_request/3, send_request/2]).

%% Custom behaviour
-export([behaviour_info/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

-record(state, {transactions = dict:new(), mod, mod_state}).
-record(req_info, {request, destinations, user_data}).

-spec behaviour_info(callbacks | term()) -> [{Function :: atom(), Arity :: integer()}].
behaviour_info(callbacks) ->
    [{handle_request, 3}, {handle_response, 3}] ++ gen_server:behaviour_info(callbacks);
behaviour_info(_) -> undefined.

%% @doc Creates UA process as part of the supervision tree
%%
%% @see gen_server:start_link/3
%% @end
-spec start_link(module(), term(), [Option :: term()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Module, Args, Options) ->
    gen_server:start_link(sip_ua, {Module, Args}, Options).

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
    RequestURI = sip_uri:parse(ToValue#sip_hdr_address.uri),

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

    Msg = #sip_message{kind = #sip_request{method = Method, uri = RequestURI},
                       headers = [Via, MaxForwards, From, To, CSeq, CallId, Contact] ++ Routes},

    Msg.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% @end
-spec send_request(term(), #sip_message{}) -> {ok, binary()}.
send_request(Request, UserData) ->
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
    Destinations = sip_resolve:client_resolve(URI2),

    % Dispatch asynchronously
    ReqInfo = #req_info{request = Request, user_data = UserData, destinations = Destinations},
    gen_server:cast(self(), {send, ReqInfo}),
    ok.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
-spec init({module(), term()}) -> {ok, #state{}}.
init({Mod, Args}) ->
    {ok, ModState} = Mod:init(Args),
    State = #state{mod = Mod, mod_state = ModState},
    {ok, State}.

%% @private
-spec handle_call(term(), term(), #state{}) -> any().
handle_call(Req, From, #state{mod = Mod, mod_state = ModState} = State) ->
    Response = Mod:handle_call(Req, From, ModState),
    wrap_response(Response, State).

%% @private
-spec handle_cast(_, #state{}) -> any().
handle_cast({send, ReqInfo}, State) ->
    next_destination(ReqInfo, State);
handle_cast(Req, #state{mod = Mod, mod_state = ModState} = State) ->
    Response = Mod:handle_cast(Req, ModState),
    wrap_response(Response, State).

%% @private
-spec handle_info(term(), #state{}) -> any().
handle_info({tx, TxKey, {response, Msg}}, #state{mod = Mod, mod_state = ModState} = State) ->
    % RFC3261 8.1.3.3 Vias
    case count_via(Msg) of
        1 ->
            % handle response from transaction layer
            ReqInfo = dict:fetch(TxKey, State#state.transactions),
            Response = Mod:handle_response(Msg, ReqInfo#req_info.user_data, ModState),
            wrap_response(Response, State);
        _Other ->
            % discard response
            error_logger:warning_report(['message_discarded',
                                         {reason, wrong_vias},
                                         {msg, Msg}]),
            {noreply, State}
    end;
handle_info({tx, TxKey, {request, Msg}}, #state{mod = Mod, mod_state = ModState} = State) ->
    % handle request from transaction layer
    ReqInfo = dict:fetch(TxKey, State#state.transactions),
    Response = Mod:handle_request(Msg, ReqInfo#req_info.user_data, ModState),
    wrap_response(Response, State);
handle_info({tx, TxKey, {terminated, normal}}, State) ->
    % remove transaction from the list
    Dict = dict:erase(TxKey, State#state.transactions),
    {noreply, State#state{transactions = Dict}};
handle_info({tx, TxKey, {terminated, Reason}}, State) ->
    ReqInfo = dict:fetch(TxKey, State#state.transactions),

    % remove transaction from the list
    Dict = dict:erase(TxKey, State#state.transactions),
    State2 = State#state{transactions = Dict},

    % transaction failure, let's retry with new transaction, see 8.1.2
    case ReqInfo#req_info.destinations of
        [] -> % if no more destinations, report as failed response
            handle_failure(Reason, ReqInfo, State2);
        _Other ->
            next_destination(ReqInfo, State2)
    end;
handle_info(Req, #state{mod = Mod, mod_state = ModState} = State) ->
    Response = Mod:handle_info(Req, ModState),
    wrap_response(Response, State).


%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(Reason, #state{mod = Mod, mod_state = ModState}) ->
    ?debugHere,
    Mod:terminate(Reason, ModState).

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(OldVsn, #state{mod = Mod} = State, Extra) ->
    {ok, ModState} = Mod:code_change(OldVsn, State#state.mod_state, Extra),
    {ok, State#state{mod_state = ModState}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
next_destination(#req_info{request = Request, destinations = [Top | Fallback]} = ReqInfo, State) ->
    % send request to the top destination
    Request2 = sip_message:update_top_header('via', fun generate_branch/1, Request),
    {ok, TxKey} = sip_transaction:start_client_tx(self(), Top, Request2),

    % store tx id to {user data, fallback destinations, request} mapping
    ReqInfo2 = ReqInfo#req_info{destinations = Fallback},
    Dict = dict:store(TxKey, ReqInfo2, State#state.transactions),
    {noreply, State#state{transactions = Dict}}.

generate_branch(Via) ->
    Branch = sip_idgen:generate_branch(),
    Params = lists:keystore(branch, 1, Via#sip_hdr_via.params, {branch, Branch}),
    Via#sip_hdr_via{params = Params}.

count_via(Msg) ->
    Fun = fun ({'via', List}, Acc) when is_list(List) -> Acc + length(List);
              ({'via', _Value}, Acc) -> Acc + 1;
              (_Header, Acc) -> Acc
          end,
    lists:foldl(Fun, 0, Msg#sip_message.headers).

%% @doc Handle failed requests (8.1.3.1, RFC 3261)
%% @end
handle_failure(Reason, ReqInfo, #state{mod = Mod, mod_state = ModState} = State) ->
    {Status, Phrase} = reason_phrase(Reason),
    Response = sip_message:create_response(ReqInfo#req_info.request, Status, Phrase),
    Result = Mod:handle_response(Response, ReqInfo#req_info.user_data, ModState),
    wrap_response(Result, State).

reason_phrase({timeout, _Timer}) -> {408, <<"Request Timeout">>};
reason_phrase({econnrefused, _}) -> {503, <<"Service Unavailable (connection refused)">>};
reason_phrase(_Reason) -> {503, <<"Service Unavailable">>}.

%% @doc Wrap client state back into the `#state{}'
%% @end
wrap_response({reply, Reply, ModState}, State) -> {reply, Reply, State#state{mod_state = ModState}};
wrap_response({reply, Reply, ModState, Timeout}, State) -> {reply, Reply, State#state{mod_state = ModState}, Timeout};
wrap_response({noreply, ModState}, State) -> {noreply, State#state{mod_state = ModState}};
wrap_response({noreply, ModState, Timeout}, State) -> {noreply, State#state{mod_state = ModState}, Timeout};
wrap_response({stop, Reason, Reply, ModState}, State) -> {stop, Reason, Reply, State#state{mod_state = ModState}};
wrap_response({stop, Reason, ModState}, State) -> {stop, Reason, State#state{mod_state = ModState}}.
