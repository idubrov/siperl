%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Base transaction functions, which are used in macroses from
%%% `sip_transaction.hrl' to simplify transactions FSM code.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_base).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").
-include("sip_transaction.hrl").

%% Exports


%% FSM callbacks
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
-export([start_timer/4, cancel_timer/2]).
-export([send_ack/2, send_request/2, send_response/2, init/1, pass_to_tu/2]).
-export([setup_loop_detection/1, is_loop_detected/1]).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec init(sip_tx_key()) -> {ok, 'INIT', undefined}.
init(TxKey) ->
    % Register transaction under its key
    % FIXME: Race condition is possible. If two messages matching same transaction
    % arrive, second one could go to the unitialized transaction.
    % This is mostly a problem for server transactions.
    gproc:add_local_name({tx, TxKey}),
    {ok, 'INIT', undefined}.

-spec cancel_timer(integer(), #tx_state{}) -> #tx_state{}.
cancel_timer(TimerIdx, TxState)
  when is_integer(TimerIdx), is_record(TxState, tx_state) ->
    case element(TimerIdx, TxState) of
        undefined ->
            TxState;

        Timer ->
            _Ignore = gen_fsm:cancel_timer(Timer),
            setelement(TimerIdx, TxState, undefined)
    end.

-spec start_timer(atom(), integer(), integer(), #tx_state{}) -> #tx_state{}.
start_timer(TimerName, TimerIdx, Interval, TxState) ->
    Timer = gen_fsm:start_timer(Interval, {TimerName, Interval}),
    setelement(TimerIdx, TxState, Timer).

-spec send_ack(#sip_response{}, #tx_state{}) -> ok.
send_ack(Response, TxState) ->
    ACK = sip_message:create_ack(TxState#tx_state.request, Response),
    send_request(ACK, TxState).

-spec send_request(#sip_request{}, #tx_state{}) -> ok.
send_request(Request, TxState) ->
    % Send request to the given destination address
    % Extract 'ttl' option from the options list
    Opts = lists:filter(fun(N) -> N =:= ttl end, TxState#tx_state.options),
    case sip_transport:send_request(TxState#tx_state.destination, Request, Opts) of
        ok -> ok;
        {error, Reason} ->
            erlang:error(Reason)
    end,
    ok.

-spec send_response(#sip_response{}, #tx_state{}) -> ok.
send_response(Response, #tx_state{tx_user = TU}) ->
    ok = case sip_transport:send_response(Response) of
        ok -> ok;

        % According to the RFC 6026, we should report to TU
        {error, Reason} ->
            TU ! {tx, self(), {error, Reason}},
            ok
    end.

-spec pass_to_tu(sip_message(), #tx_state{}) -> ok.
pass_to_tu(_Message, #tx_state{tx_user = none}) -> ok; % No TU to report to
pass_to_tu(#sip_request{} = Msg, #tx_state{tx_user = TU}) ->
    TU ! {request, Msg, self()},
    ok;
pass_to_tu(#sip_response{} = Msg, #tx_state{tx_user = TU}) ->
    TU ! {response, Msg, self()},
    ok.

%% @private
-spec handle_event(term(), atom(), #tx_state{}) ->
          {stop, term(), #tx_state{}}.
handle_event(Event, _State, TxState) ->
    {stop, {unexpected, Event}, TxState}.

%% @private
-spec handle_sync_event(term(), term(), atom(), #tx_state{}) ->
          {stop, term(), term(), #tx_state{}}.
handle_sync_event(Event, _From, _State, TxState) ->
    Reason = {unexpected, Event},
    {stop, Reason, Reason, TxState}.

%% @private
-spec handle_info(term(), atom(), #tx_state{}) -> {stop, term(), #tx_state{}}.
handle_info({error, Reason}, _State, #tx_state{tx_key = #sip_tx_client{}} = TxState) ->
    % sent by transport implementation to all processes with gproc property `{icmp, econnrefused, Addr, Port}'
    {stop, {error, Reason}, TxState};

handle_info({error, Reason}, _State, #tx_state{tx_key = #sip_tx_server{}, tx_user = TU} = TxState) ->
    % according to the RFC 6026, we should report to TU
    TU ! {tx, self(), {error, Reason}},
    {noreply, TxState};

handle_info(Info, _State, TxState) ->
    {stop, {unexpected, Info}, TxState}.

%% @doc
%% Inform the transaction user about transition to 'TERMINATED' state.
%% @end
-spec terminate(term(), atom(), #tx_state{}) -> ok.
terminate(_Reason, _State, undefined) ->
    % Not yet initialized
    ok;
terminate(Reason, _State, TxState) ->
    % For client transactions, handle failure as response message
    % See 8.1.3.1 (Transaction Layer Errors)
    % FIXME: What if this message will be processed by UA after 'DOWN' event?
    % Probably, UA should handle 'DOWN' events itself?
    ok =
        case failed_status(TxState#tx_state.tx_key, Reason) of
            false -> ok;
            Status ->
                % Send failure as response to the TU
                Response = sip_message:create_response(TxState#tx_state.request, Status),
                ok = pass_to_tu(Response, TxState),
                ok
        end,
    ok.

%% @doc Determine if response should be sent to TU
%%
%% Syntetic response is sent when client transaction terminated abnormally.
%% If reason is timeout, "408 Request Timeout" is sent. Otherwise, "503 Service Unavailable"
%% is sent.
%% @end
failed_status(#sip_tx_server{}, _Reason) -> false;
failed_status(#sip_tx_client{}, normal) -> false;
failed_status(#sip_tx_client{}, {timeout, _Timer}) -> 408;
failed_status(#sip_tx_client{}, _Reason) -> 503.

%% @private
-spec code_change(term(), atom(), #tx_state{}, term()) -> {ok, atom(), #tx_state{}}.
code_change(_OldVsn, State, TxState, _Extra) ->
    {ok, State, TxState}.

-spec setup_loop_detection(#tx_state{}) -> ok.
%% @doc Setup loop detection (see RFC 3261 8.2.2.2)
%% Implemented on transaction side since it requires several properties of transaction
%% not available to UA.
%% @end
setup_loop_detection(#tx_state{request = Request, tx_key = TxKey}) ->
    % Add gproc: property for loop detection for server transactions, see 8.2.2.2
    From = sip_message:header_top_value(from, Request),
    case lists:keyfind(tag, 1, From#sip_hdr_address.params) of
        {tag, FromTag} ->
            CallId = sip_message:header_top_value('call-id', Request),
            CSeq = sip_message:header_top_value(cseq, Request),
            Key = {tx_loop, FromTag, CallId, CSeq},
            true = gproc:add_local_property(Key, TxKey),
            ok;
        false ->
            ok
    end.

%% @doc Check message against loop conditions
%%
%% Check if loop is detected by by following procedures from 8.2.2.2
%% @end
-spec is_loop_detected(#sip_request{}) -> boolean().
is_loop_detected(#sip_request{} = Request) ->
    To = sip_message:header_top_value(to, Request),

    case lists:keyfind(tag, 1, To#sip_hdr_address.params) of
        false ->
            TxKey = sip_transaction:tx_key(server, Request),

            From = sip_message:header_top_value(from, Request),
            {tag, FromTag} = lists:keyfind(tag, 1, From#sip_hdr_address.params),

            CallId = sip_message:header_top_value('call-id', Request),
            CSeq = sip_message:header_top_value(cseq, Request),
            Key = {tx_loop, FromTag, CallId, CSeq},
            List = gproc:lookup_local_properties(Key),
            case List of
                % either no transactions with same From: tag, Call-Id and CSeq
                % or there is one such transaction and message matches it
                [] -> false;
                [{_Pid, TxKey}] -> false;
                % there are transactions that have same From: tag, Call-Id and CSeq,
                % but message does not matches them --> loop detected
                _Other -> true
            end;
        % tag present, no loop
        {tag, _Tag} -> false
    end.
