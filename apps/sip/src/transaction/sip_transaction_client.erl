%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% RFC 3261 17.1.2 Non-INVITE Client Transaction
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3261</a>.
%%% @reference See <a href="http://tools.ietf.org/html/rfc6026">RFC 6026</a>.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_client).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip.hrl").
-include("sip_transaction.hrl").

%% FSM callbacks (the rest are provided by `sip_transaction_base')
-export(['INIT'/2, 'TRYING'/2, 'PROCEEDING'/2, 'COMPLETED'/2]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
%% @doc `INIT' state is for heavy-weight initialization (sending request, starting timers)
%% @end
-spec 'INIT'({init, #tx_state{}}, undefined) -> {next_state, 'TRYING', #tx_state{}}.
'INIT'({init, TxState}, undefined) ->
    true = gproc:mreg(p, l, TxState#tx_state.props),

    % start Timer E only for unreliable transports
    IsReliable = TxState#tx_state.reliable,
    TxState2 = case IsReliable of
                true -> TxState;
                false -> ?START(timerE, ?T1, TxState)
            end,
    % timer F
    TxState3 = ?START(timerF, 64 * ?T1, TxState2),
    % send request
    ok = sip_transaction_base:send_request(TxState3#tx_state.request, TxState3),
    {next_state, 'TRYING', TxState3}.

-spec 'TRYING'(term(), #tx_state{}) -> term().
%% @doc Handle retransmission timer (Timer E).
%% @end
'TRYING'({timeout, _Ref, {timerE, Interval}}, TxState) ->
    ok = sip_transaction_base:send_request(TxState#tx_state.request, TxState),

    % request is retransmitted with intervals that double after each transmission
    NewInterval = min(2 * Interval, ?T2),
    TxState2 = ?START(timerE, NewInterval, TxState),
    {next_state, 'TRYING', TxState2};

%% @doc
%% Transaction timed out.
%% @end
'TRYING'({timeout, _Ref, {timerF, _}}, TxState) ->
    {stop, {timeout, timerF}, TxState};

%% @doc
%% Handle provisional (1xx) responses. This handles both 'TRYING' and 'PROCEEDING'
%% states as they are similar.
%% @end
'TRYING'({response, Status, Response}, TxState)
  when Status >= 100, Status =< 199 ->
    %% Provisional response, transition to PROCEEDING state.

    ok = sip_transaction_base:pass_to_tu(Response, TxState),
    {next_state, 'PROCEEDING', TxState};

'TRYING'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 699 ->
    %% Final response, transition to COMPLETED state.

    ok = sip_transaction_base:pass_to_tu(Response, TxState),

    % cancel the timers E and F
    TxState2 = ?CANCEL(timerE, TxState),
    TxState3 = ?CANCEL(timerF, TxState2),

    % start timer K
    case TxState3#tx_state.reliable of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, TxState3};
        false ->
            TxState4 = ?START(timerK, ?T4, TxState3),
            {next_state, 'COMPLETED', TxState4}
    end.

-spec 'PROCEEDING'(term(), #tx_state{}) -> term().
%% @doc In 'PROCEEDING' state, act the same way as in 'TRYING' state.
%% @end
'PROCEEDING'({response, Status, Msg}, TxState) ->
    'TRYING'({response, Status, Msg}, TxState);

%% @doc If Timer E fires while in the "Proceeding" state, the request MUST be
%% passed to the transport layer for retransmission, and Timer E MUST be
%% reset with a value of T2 seconds.
%% @end

'PROCEEDING'({timeout, _Ref, {timerE, _Interval}}, TxState) ->
    ok = sip_transaction_base:send_request(TxState#tx_state.request, TxState),

    TxState2 = ?START(timerE, ?T2, TxState),
    {next_state, 'PROCEEDING', TxState2};

%% @doc Transaction timed out.
%% @end
'PROCEEDING'({timeout, _Ref, {timerF, _}}, TxState) ->
    {stop, {timeout, timerF}, TxState}.

-spec 'COMPLETED'(term(), #tx_state{}) -> term().
%% @doc Buffer additional response retransmissions.
%% @end
'COMPLETED'({response, _Status, _Response}, TxState) ->
    {next_state, 'COMPLETED', TxState};

%% @doc
%% When Timer K fires while in 'COMPLETED' state, transition to the 'TERMINATED'
%% state.
%% @end
'COMPLETED'({timeout, _Ref, {timerK, _}}, TxState) ->
    {stop, normal, TxState}.
