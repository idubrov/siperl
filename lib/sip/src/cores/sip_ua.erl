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
-export([create_request/3, send_request/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

-record(state, {sending = [], callback, cstate}).

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
-spec create_request(sip_headers:method(), sip_headers:address(), sip_headers:address()) -> sip_message:message().
create_request(Method, ToValue, ContactValue) when
  is_record(ToValue, sip_hdr_address), is_record(ContactValue, sip_hdr_address) ->
    % The initial Request-URI of the message SHOULD be set to the value of
    % the URI in the To field.
    RequestURI = ToValue#sip_hdr_address.uri,

    % will be updated later (by transport layer)
    Via = {'via', sip_headers:via(undefined, undefined, [{branch, sip_idgen:generate_branch()}])},
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
    Routes = [{'route', sip_headers:route(Route, [])} || Route <- sip_config:configured_routes()],

    Msg = #sip_message{start_line = {request, Method, RequestURI},
                       headers = [Via, MaxForwards, From, To, CSeq, CallId, Contact] ++ Routes},

    Msg.

%% @doc Send the request according to the 8.1.2 Sending the Request
%% @end
-spec send_request(sip_message:message()) -> {ok, binary()}.
send_request(Request) ->
    {request, _Method, RequestURI} = Request#sip_message.start_line,
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
    % FIXME: if the Request-URI specifies a SIPS resource, consider URI to be SIPS as well
    Destinations = sip_resolve:client_resolve(URI),

    % FIXME: stateful element, call self to send the request...
    % Generate id that UAC could use to identify the request being sent
    Id = sip_idgen:generate_id(8),
    gen_server:cast(self(), {send, Id, Destinations, Request}),
    {ok, Id}.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
-spec init({module(), term()}) -> {ok, #state{}}.
init({Callback, Args}) ->
    {ok, CState} = Callback:init(Args),
    State = #state{callback = Callback, cstate = CState},
    {ok, State}.

%% @private
-spec handle_call(term(), term(), #state{}) -> any().
handle_call(Req, From, State) ->
    Callback = State#state.callback,
    Response = Callback:handle_call(Req, From, State#state.cstate),
    handle_response(Response, State).

%% @private
-spec handle_cast(_, #state{}) -> any().
handle_cast({send, Id, Destinations, Request}, State) ->
    State2 = send_internal(Id, Destinations, Request, State),
    {noreply, State2};
handle_cast(Req, State) ->
    Callback = State#state.callback,
    Response = Callback:handle_cast(Req, State#state.cstate),
    handle_response(Response, State).

%% @private
-spec handle_info(term(), #state{}) -> any().
handle_info({tx, TxKey, {terminated, normal}}, State) ->
    Sending = lists:keydelete(TxKey, 2, State#state.sending),
    {noreply, State#state{sending = Sending}};
handle_info({tx, TxKey, {terminated, _Reason}}, State) ->
    % transaction failure, let's retry with new transaction, see 8.1.2
    {Id, TxKey, Fallback, Request} = lists:keyfind(TxKey, 2, State#state.sending),
    State2 = send_internal(Id, Fallback, Request, State),
    {noreply, State2};
handle_info(Req, State) ->
    Callback = State#state.callback,
    Response = Callback:handle_info(Req, State#state.cstate),
    handle_response(Response, State).


%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    Callback = State#state.callback,
    Callback:terminate(Reason, State#state.cstate).

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(OldVsn, State, Extra) ->
    Callback = State#state.callback,
    {ok, CState} = Callback:code_change(OldVsn, State#state.cstate, Extra),
    {ok, State#state{cstate = CState}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc Send request to destination on the top
%%
%% If no destinations are provided, report error to the callback module.
%% @end
send_internal(Id, [], Request, State) ->
    % no more destinations -- report to callback
    Callback = State#state.callback,
    Callback:request_failed(Id, Request, request_timeout),
    State;
send_internal(Id, [Top|Fallback], Request, State) ->
    % send request to the top destination, remember the transaction id,
    % remember the rest as fallback destinations
    Request2 = sip_message:update_top_header('via', fun generate_branch/1, Request),
    {ok, TxKey} = sip_transaction:start_client_tx(self(), Top, Request2),
    Sending = lists:keystore(Id, 1, State#state.sending, {Id, TxKey, Fallback, Request}),
    State#state{sending = Sending}.

generate_branch(Via) ->
    Branch = sip_idgen:generate_branch(),
    Params = lists:keystore(branch, 1, Via#sip_hdr_via.params, {branch, Branch}),
    Via#sip_hdr_via{params = Params}.


%% @doc Wrap client state back into the `#state{}'
%% @end
handle_response({reply, Reply, CState}, State) -> {reply, Reply, State#state{cstate = CState}};
handle_response({reply, Reply, CState, Timeout}, State) -> {reply, Reply, State#state{cstate = CState}, Timeout};
handle_response({noreply, CState}, State) -> {noreply, State#state{cstate = CState}};
handle_response({noreply, CState, Timeout}, State) -> {noreply, State#state{cstate = CState}, Timeout};
handle_response({stop, Reason, Reply, CState}, State) -> {stop, Reason, Reply, State#state{cstate = CState}};
handle_response({stop, Reason, CState}, State) -> {stop, Reason, State#state{cstate = CState}}.
