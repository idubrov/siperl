%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS server
%%%
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
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua).
-compile({parse_transform, do}).

%% API
-export([start_link/3, start_link/4]).
-export([create_request/2, create_ack/1, send_request/1, cancel_request/1]). % UAC
-export([create_response/2, create_response/3, send_response/2]). % UAS

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-define(CALLBACK, sip_ua_callback). % Key in the process dictionary for callback module name

-type state() :: term().     % Callback module state
-type gen_from() :: {pid(), term()}.

-type options() :: [no_detect_loops].

%% API

-spec start_link({global,_} | {local, atom()}, module(), term(), options()) -> {ok, pid()} | {error, term()}.
start_link(Name, Callback, Args, Opts) when is_atom(Callback), is_list(Opts) ->
    gen_server:start_link(Name, ?MODULE, {Callback, Args, Opts}, []).

-spec start_link(module(), term(), options()) -> {ok, pid()} | {error, term()}.
start_link(Callback, Args, Opts) when is_atom(Callback), is_list(Opts) ->
    gen_server:start_link(?MODULE, {Callback, Args, Opts}, []).

%% @doc Create request outside of the dialog according to the 8.1.1 Generating the Request
%%
%% Creates all required headers of the SIP message: `Via:', `Max-Forwards:',
%% `From:', `To:', `CSeq:', `Call-Id'. Also, adds `Route:' headers
%% if pre-existing route set is configured.
%%
%% Clients are free to modify any part of the request according to their needs.
%% @end
-spec create_request(sip_name(), #sip_hdr_address{} | #sip_dialog_id{}) -> #sip_request{}.
create_request(Method, To) when is_record(To, sip_hdr_address) ->
    sip_ua_client:create_request(Method, To);

%% @doc Create request within the  dialog according to the 12.2.1.1 Generating the Request
%% Clients are free to modify any part of the request according to their needs.
%% @end
create_request(Method, Dialog) when is_record(Dialog, sip_dialog_id) ->
    sip_ua_client:create_request(Method, Dialog).

%% @doc Create ACK based on the 2xx response to the INVITE message
%% FIXME: UAC should create and send ACK automatically?
%% @end
-spec create_ack(#sip_response{}) -> #sip_request{}.
create_ack(Response) when is_record(Response, sip_response) ->
    sip_ua_client:create_ack(Response).

%% @doc Create request outside of the dialog according to the 8.2.6 Generating the Response
%% @end
-spec create_response(#sip_request{}, integer()) -> #sip_response{}.
create_response(Request, Status) ->
    sip_ua_server:create_response(Request, Status).

%% @doc Create request outside of the dialog according to the 8.2.6 Generating the Response
%% @end
-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(Request, Status, Reason) ->
    sip_ua_server:create_response(Request, Status, Reason).

%% @doc Send the request
%%
%% The request is processed only when callback returns the control to the `sip_ua'
%% generic server. Responses are provided via `Callback:handle_response/4' function.
%% <em>Should be called from UAC/UAS process only</em>
%% @end
-spec send_request(#sip_request{}) -> {ok, reference()}.
send_request(Request) when is_record(Request, sip_request) ->
    sip_ua_client:send_request(Request).

-spec cancel_request(reference()) -> ok | {error, no_request}.
%% @doc Cancel the request identified by the reference
%% <em>Note that it is still possible for the client to receive 2xx response
%% on the request that was successfully cancelled. This is due to the inherent
%% race condition present. For example, this could happen if cancel is invoked
%% before UAC have received 2xx response, but after it was sent by the remote side.
%% That means, client should be ready to issue `BYE' when 2xx is received on
%% request it has cancelled.</em>.
%% <em>Should be called from UAC/UAS process only</em>
%% @end
cancel_request(Id) when is_reference(Id) ->
    sip_ua_client:cancel_request(Id).

-spec send_response(#sip_request{}, #sip_response{}) -> ok | {error, Reason :: term()}.
send_response(Request, Response) ->
    Callback = callback(),
    sip_ua_server:send_response(Request, Response, Callback).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({module(), term(), options()}) -> {ok, state()}.
init({Callback, Args, Opts}) ->
    ok = sip_ua_client:init(Opts),
    ok = sip_ua_server:init(Opts),

    {ok, State} = Callback:init(Args),

    erlang:put(?CALLBACK, Callback),

    % according to RFC 6026 8.9 stray responses MUST be dropped
    IsApplicable =
        fun(#sip_response{}) -> false;
           (#sip_request{} = Request) -> Callback:is_applicable(Request)
        end,
    sip_cores:register_core(#sip_core_info{is_applicable = IsApplicable}),
    {ok, State}.

%% @private
-spec handle_call(term(), gen_from(), state()) -> {stop, {unexpected, term()}, state()}.
handle_call(Req, From, State) ->
    Callback = callback(),
    Callback:handle_call(Req, From, State).

%% @private
-spec handle_cast(term(), state()) ->
          {noreply, state()} |
          {stop, Reason :: term(), state()}.
handle_cast(Cast, State) ->
    Callback = callback(),
    Callback:handle_cast(Cast, State).

%% @private
-spec handle_info(_, state()) -> {noreply, state()}.
handle_info({response, #sip_response{} = Response, TxPid}, State) when is_pid(TxPid) ->
    % pass responses to UAC
    Callback = callback(),
    sip_ua_client:handle_response(Response, TxPid, Callback, State);

handle_info({request, #sip_request{} = Request, _TxPid}, State) ->
    % this must be ACK (see RFC 6026), pass it to UAS
    Callback = callback(),
    sip_ua_server:handle_request(Request, Callback, State);
handle_info({request, #sip_request{} = Request}, State) ->
    % pass requests to UAS
    Callback = callback(),
    sip_ua_server:handle_request(Request, Callback, State);
handle_info({tx, TxPid, {error, Reason}}, State) ->
    % Transport error is reported by server transaction, pass to UAS
    Callback = callback(),
    sip_ua_server:handle_error(Reason, TxPid, Callback, State);
handle_info({cancel_request, Id}, State) when is_reference(Id) ->
    % this message is triggered by the timer from sip_ua_client which is set
    % when Expires: header present in the initial INVITE
    _Ignore = sip_ua_client:cancel_request(Id),
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, Info}, State) ->
    case sip_ua_client:handle_down(Ref, Pid, Info) of
        true -> {noreply, State};
        false ->
            % let the callback deal with that info
            Callback = callback(),
            Callback:handle_info(Info, State)
    end;
handle_info(Info, State) ->
    Callback = callback(),
    Callback:handle_info(Info, State).

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


%% @doc Get callback module from the process registry
%% Fail if callback is not set (wrong process is used)
%% @end
callback() ->
    case erlang:get(?CALLBACK) of
        undefined ->
            erlang:exit(wrong_process);
        Callback ->
            Callback
    end.

