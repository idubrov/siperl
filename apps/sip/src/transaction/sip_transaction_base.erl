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

-spec send_ack(#sip_response{}, #tx_state{}) -> #tx_state{}.
send_ack(Response, TxState) ->
    ACK = sip_message:create_ack(TxState#tx_state.request, Response),
    send_request(ACK, TxState).

-spec send_request(#sip_request{}, #tx_state{}) -> #tx_state{}.
send_request(Request, TxState) ->
    % Send request to the given destination address
    % Extract 'ttl' option from the options list
    Opts = lists:filter(fun(N) -> N =:= ttl end, TxState#tx_state.options),
    case sip_transport:send_request(TxState#tx_state.destination, Request, Opts) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end,
    TxState.

-spec send_response(#sip_response{}, #tx_state{}) -> #tx_state{}.
send_response(Response, TxState) ->
    case sip_transport:send_response(Response) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end,
    TxState.

-spec pass_to_tu(#sip_response{}, #tx_state{}) -> ok.
pass_to_tu(#sip_response{}, #tx_state{tx_user = none}) -> ok; % No TU to report to
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
    % See 8.1.3.1 (Transaction Layer Errors), 9.1 (Cancelling a Request)
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
