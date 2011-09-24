%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% RFC 3261 17.1.1 INVITE Client Transaction
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3261</a>.
%%% @reference See <a href="http://tools.ietf.org/html/rfc6026">RFC 6026</a>.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_client_invite).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip.hrl").
-include("sip_transaction.hrl").

%% FSM callbacks (the rest are provided by `sip_transaction_base')
-export(['INIT'/2, 'CALLING'/2, 'PROCEEDING'/2, 'COMPLETED'/2, 'ACCEPTED'/2]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------


%% @doc
%% `INIT' state is for heavy-weight initialization (sending request, starting timers)
%% @end
-spec 'INIT'({init, #tx_state{}}, undefined) -> {next_state, 'CALLING', #tx_state{}}.
'INIT'({init, TxState}, undefined) ->
    gproc:mreg(p, l, TxState#tx_state.props),

    % start Timer A only for unreliable transports
    IsReliable = TxState#tx_state.reliable,
    TxState2 = case IsReliable of
                true -> TxState;
                false -> ?START(timerA, ?T1, TxState)
            end,
    % timer B
    TxState3 = ?START(timerB, 64 * ?T1, TxState2),
    % send request
    ok = sip_transaction_base:send_request(TxState3#tx_state.request, TxState3),
    {next_state, 'CALLING', TxState3}.

-spec 'CALLING'(term(), #tx_state{}) -> term().
%% @doc
%% Handle retransmission timer (Timer A).
%% @end
'CALLING'({timeout, _Ref, {timerA, Interval}}, TxState) ->
    ok = sip_transaction_base:send_request(TxState#tx_state.request, TxState),

    % request is retransmitted with intervals that double after each transmission
    TxState2 = ?START(timerA, 2 * Interval, TxState),
    {next_state, 'CALLING', TxState2};

%% @doc Transaction timed out.
%% @end
'CALLING'({timeout, _Ref, {timerB, _}}, TxState) ->
    {stop, {timeout, timerB}, TxState};

%% @doc
%% Handle provisional (1xx) responses. This handles both 'CALLING' and 'PROCEEDING'
%% states as they are similar.
%% @end
'CALLING'({response, Status, Response}, TxState)
  when Status >= 100, Status =< 199 ->
    %% Provisional response, transition to PROCEEDING state.

    ok = sip_transaction_base:pass_to_tu(Response, TxState),
    % stop retransmissions in 'PROCEEDING' state
    TxState2 = ?CANCEL(timerA, TxState),
    {next_state, 'PROCEEDING', TxState2};

%% @doc Error response, transition to COMPLETED/TERMINATED state.
%% @end
'CALLING'({response, Status, Response}, TxState)
  when Status >= 300, Status =< 699 ->

    ok = sip_transaction_base:pass_to_tu(Response, TxState),
    ok = sip_transaction_base:send_ack(Response, TxState),

    % cancel the timers A and B
    TxState2 = ?CANCEL(timerA, TxState),
    TxState3 = ?CANCEL(timerB, TxState2),

    % start timer D (for unreliable)
    case TxState3#tx_state.reliable of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, TxState3};
        false ->
            TxState4 = ?START(timerD, 32000, TxState3),
            {next_state, 'COMPLETED', TxState4}
    end;

%% @doc When a 2xx response is received while in either the 'CALLING' or
%% 'PROCEEDING' states, the client transaction MUST transition to the
%% 'ACCEPTED' state.
%% @end
'CALLING'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 299 ->
    % pass response to TU
    ok = sip_transaction_base:pass_to_tu(Response, TxState),

    % start timer M, cancel timer A and B
    TxState2 = ?START(timerM, 64 * ?T1, TxState),
    TxState3 = ?CANCEL(timerA, TxState2),
    TxState4 = ?CANCEL(timerB, TxState3),

    {next_state, 'ACCEPTED', TxState4}.

-spec 'PROCEEDING'(term(), #tx_state{}) -> term().
%% @doc
%% In 'PROCEEDING' state, act the same way as in 'CALLING' state.
%% The only difference is Timer A which is cancelled during transition
%% to the PROCEEDING state.
%% @end
'PROCEEDING'({response, Status, Msg}, TxState) ->
    'CALLING'({response, Status, Msg}, TxState);

%% @doc Transaction timed out
%% Same handling as in `CALLING' state.
%% @end
'PROCEEDING'({timeout, Ref, {timerB, Interval}}, TxState) ->
    'CALLING'({timeout, Ref, {timerB, Interval}}, TxState).

-spec 'COMPLETED'(term(), #tx_state{}) -> term().
%% @doc In 'COMPLETED' state transaction re-sends ACK without passing the response
%% to the Transaction User
%% @end
'COMPLETED'({response, _Status, Response}, TxState) ->
    ok = sip_transaction_base:send_ack(Response, TxState),
    {next_state, 'COMPLETED', TxState};

%% @doc When Timer D fires while in 'COMPLETED' state, transition to the 'TERMINATED'
%% state.
%% @end
'COMPLETED'({timeout, _Ref, {timerD, _}}, TxState) ->
    {stop, normal, TxState}.

-spec 'ACCEPTED'(term(), #tx_state{}) -> term().
%% @doc Any 2xx responses that match this client transaction and that are
%% received while in the 'ACCEPTED' state MUST be passed up to the TU.
%% @end
'ACCEPTED'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 299 ->
    % pass response to TU
    ok = sip_transaction_base:pass_to_tu(Response, TxState),
    {next_state, 'ACCEPTED', TxState};

%% @doc If Timer M fires while the client transaction is in the 'ACCEPTED'
%% state, the client transaction MUST move to the 'TERMINATED' state.
%% @end
'ACCEPTED'({timeout, _Ref, {timerM, _}}, TxState) ->
    {stop, normal, TxState}.
