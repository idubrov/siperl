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
-export([start_link/0, create_request/3, send_request/2, send_request/3, send_request_sync/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-record(req_info, {request :: #sip_request{},                 % SIP request message
                   destinations = [] :: [#sip_destination{}], % list of IP addresses to try next
                   target_set = sip_priority_set:new(),       % URI to visit next (redirects)
                   user_data}).                               % Custom user data associated with request

-record(state, {}).

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
-spec send_request(atom() | pid(), sip_message(), fun((#sip_response{}) -> ok)) -> ok.
send_request(UAC, Request, Callback) when is_record(Request, sip_request), is_function(Callback, 1) ->
    ok = gen_server:cast(UAC, {send_request, Request, Callback}).

%% @doc Send the request asynchronously. Responses will be provided via
%% `{response, Response}' messages delivered to the caller
%% @end
-spec send_request(atom() | pid(), sip_message()) -> ok.
send_request(UAC, Request) when is_record(Request, sip_request) ->
    Pid = self(),
    Callback = fun({ok, Response}) -> Pid ! {response, Response} end,
    ok = gen_server:cast(UAC, {send_request, Request, Callback}).


%% @doc Send the request synchronously
%% FIXME: Does not work with provisional responses!!!
%% @end
-spec send_request_sync(atom() | pid(), sip_message()) -> {ok, #sip_response{}} | {error, Reason :: term()}.
send_request_sync(UAC, Request) when is_record(Request, sip_request) ->
    gen_server:call(UAC, {send_request, Request}).

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
    Callback = fun(Result) -> gen_server:reply(From, Result) end,
    ok = do_send_request(Request, Callback),
    {noreply, State};
handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({send_request, Request, Callback}, State) ->
    ok = do_send_request(Request, Callback),
    {noreply, State};
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}}.
handle_info({response, Response, ReqInfo}, State) ->
    do_response(Response, ReqInfo),
    {noreply, State};
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
do_send_request(Request, Callback) ->
    ok = sip_message:validate_request(Request),

    RequestURI = Request#sip_request.uri,
    TargetSet = sip_priority_set:put(RequestURI, 1.0, sip_priority_set:new()),

    ReqInfo = #req_info{request = Request,
                        user_data = Callback,
                        target_set = TargetSet},
    % What if `ok' is returned (meaning request was not processed)?
    {error, _Reason} = next_uri(ReqInfo, none),
    ok.

do_response(Response, ReqInfo) ->
    Result = do([error_m ||
                 validate_vias(Response),
                 handle_response(ReqInfo, Response),
                 return(ok)]),
    case Result of
        ok ->
            Callback = ReqInfo#req_info.user_data,
            Callback({ok, Response});
        {error, _Reason} ->
            % Response was either discarded or is still being
            % processed (following redirect, trying next IP, etc)
            % So, nothing to report to the client callback
            ok
    end,
    ok.

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

%% @doc Automatic handling of 3xx-6xx responses valve
%% @end
-spec handle_response(#req_info{}, #sip_response{}) -> error_m:monad(ok).
handle_response(cancel, #sip_response{}) ->
    % ignore responses for CANCEL trasactions
    error_m:fail(ignore_cancel);
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

    %% FIXME: Every new client transaction must have its own branch value!!!!!
%%     Request2 = sip_message:with_branch(sip_idgen:generate_branch(), Request),
    %%
    {ok, _TxKey} = sip_transaction:start_client_tx(self(), Top, Request, [{user_data, ReqInfo2}]),

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
    NewTargetSet = sip_message:foldl_headers('contact', CollectFun, ReqInfo#req_info.target_set, Response),
    ReqInfo#req_info{target_set = NewTargetSet, destinations = []}.
