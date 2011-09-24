%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% RFC 3261 17.2.1 INVITE Server Transaction
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3261</a>.
%%% @reference See <a href="http://tools.ietf.org/html/rfc6026">RFC 6026</a>.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_server_invite).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip.hrl").
-include("sip_transaction.hrl").

%% FSM callbacks (the rest are provided by `sip_transaction_base')
-export(['INIT'/2, 'PROCEEDING'/2, 'COMPLETED'/2, 'CONFIRMED'/2, 'ACCEPTED'/2]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
%% @doc `INIT' state is for heavy-weight initialization (sending request, starting timers)
%% @end
-spec 'INIT'({init, #tx_state{}}, undefined) -> {next_state, 'PROCEEDING', #tx_state{}}.
'INIT'({init, TxState}, undefined) ->
    gproc:mreg(p, l, TxState#tx_state.props),

    % send provisional response
    Trying = sip_message:create_response(TxState#tx_state.request, 100),
    TxState2 = TxState#tx_state{provisional = Trying},

    ok = sip_transaction_base:send_response(Trying, TxState2),
    {next_state, 'PROCEEDING', TxState2}.

-spec 'PROCEEDING'(term(), #tx_state{}) -> term().
%% @doc If a request retransmission is received while in the "Proceeding" state, the
%% most recent provisional response that was received from the TU MUST be passed
%% to the transport layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, TxState) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    ok = sip_transaction_base:send_response(TxState#tx_state.provisional, TxState),
    {next_state, 'PROCEEDING', TxState};

%% @doc
%% The TU passes any number of provisional responses to the server
%% transaction.  So long as the server transaction is in the
%% "Proceeding" state, each of these MUST be passed to the transport
%% layer for transmission.
%% @end
'PROCEEDING'({response, Status, Provisional}, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Provisional},
    ok = sip_transaction_base:send_response(Provisional, TxState2),
    {next_state, 'PROCEEDING', TxState2};

%% @doc If, while in the "Proceeding" state, the TU passes a 2xx response to
%% the server transaction, the server transaction MUST pass this
%% response to the transport layer for transmission.  It is not
%% retransmitted by the server transaction; retransmissions of 2xx
%% responses are handled by the TU.  The server transaction MUST then
%% transition to the "Accepted" state (RFC 6026).
%% @end
'PROCEEDING'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 299 ->

    ok = sip_transaction_base:send_response(Response, TxState),

    % Start timer L
    TxState2 = ?START(timerL, 64 * ?T1, TxState),
    {next_state, 'ACCEPTED', TxState2};

%% @doc
%% While in the "Proceeding" state, if the TU passes a response with
%% status code from 300 to 699 to the server transaction, the response
%% MUST be passed to the transport layer for transmission, and the state
%% machine MUST enter the "Completed" state.  For unreliable transports,
%% timer G is set to fire in T1 seconds, and is not set to fire for
%% reliable transports.
%%
%% When the "Completed" state is entered, timer H MUST be set to fire in
%% 64*T1 seconds for all transports.  Timer H determines when the server
%% transaction abandons retransmitting the response.
%% @end
'PROCEEDING'({response, Status, Response}, TxState)
  when Status >= 300, Status =< 699 ->

    TxState2 = TxState#tx_state{response = Response},
    ok = sip_transaction_base:send_response(Response, TxState2),

    % start Timer G only for unreliable transports
    IsReliable = TxState2#tx_state.reliable,
    TxState3 =
        case IsReliable of
            true -> TxState2;
            false -> ?START(timerG, ?T1, TxState2)
        end,
    % start Timer H
    TxState4 = ?START(timerH, 64 * ?T1, TxState3),
    {next_state, 'COMPLETED', TxState4};

%% @doc Transaction cancellation, Section 9.2
%% Sends "487 Request Terminated"
%% @end
'PROCEEDING'(cancel, TxState) ->
    % Simply act as we have received final response from the TU
    Response = sip_message:create_response(TxState#tx_state.request, 487),
    'PROCEEDING'({response, 487, Response}, TxState).

%% @doc
%% If timer G fires, the response is passed to the transport layer once
%% more for retransmission, and timer G is set to fire in MIN(2*T1, T2)
%% seconds.
%% @end
-spec 'COMPLETED'(term(), #tx_state{}) -> term().
'COMPLETED'({timeout, _Ref, {timerG, Interval}}, TxState) ->
    ok = sip_transaction_base:send_response(TxState#tx_state.response, TxState),

    % Re-start timer G
    NewInterval = min(2 * Interval, ?T2),
    TxState2 = ?START(timerG, NewInterval, TxState),
    {next_state, 'COMPLETED', TxState2};

%% @doc
%% If timer H fires while in the "Completed" state, it implies that the
%% ACK was never received.  In this case, the server transaction MUST
%% transition to the "Terminated" state, and MUST indicate to the TU
%% that a transaction failure has occurred.
%% @end
'COMPLETED'({timeout, _Ref, {timerH, _Interval}}, TxState) ->
    {stop, {timeout, timerH}, TxState};

%% @doc
%%  If an ACK is received while the server transaction is in the
%% "Completed" state, the server transaction MUST transition to the
%% "Confirmed" state.  As Timer G is ignored in this state, any
%% retransmissions of the response will cease.
%% @end
'COMPLETED'({request, 'ACK', _Request}, TxState) ->
    % cancel timerG
    TxState2 = ?CANCEL(timerG, TxState),

    % start timer I (only for unreliable)
    case TxState2#tx_state.reliable of
        true ->
            % skip CONFIRMED state and proceed immediately to TERMINATED state
            {stop, normal, TxState2};
        false ->
            TxState3 = ?START(timerI, ?T4, TxState2),
            {next_state, 'CONFIRMED', TxState3}
    end;

%% @doc Furthermore, while in the "Completed" state, if a request retransmission
%% is received, the server SHOULD pass the response to the transport for
%% retransmission.
%% @end
'COMPLETED'({request, _Method, _Request}, TxState) ->
    ok = sip_transaction_base:send_response(TxState#tx_state.response, TxState),
    {next_state, 'COMPLETED', TxState};

%% @doc Transaction cancellation, Section 9.2
%% Effectively does nothing, as we have already sent the final response
%% @end
'COMPLETED'(cancel, TxState) ->
    {next_state, 'COMPLETED', TxState}.

%% @doc
%% The purpose of the "Confirmed" state is to absorb any additional ACK
%% messages that arrive, triggered from retransmissions of the final
%% response.
%% state.
%% @end
-spec 'CONFIRMED'(term(), #tx_state{}) -> term().
'CONFIRMED'({timeout, _Ref, {timerI, _}}, TxState) ->
    {stop, normal, TxState};

%% @doc
%% Absorb any additional ACK messages
%% @end
'CONFIRMED'({request, _Method, _Response}, TxState) ->
    {next_state, 'CONFIRMED', TxState};

%% @doc Transaction cancellation, Section 9.2
%% Effectively does nothing, as we have already sent the final response
%% @end
'CONFIRMED'(cancel, TxState) ->
    {next_state, 'CONFIRMED', TxState}.

-spec 'ACCEPTED'(term(), #tx_state{}) -> term().
%% @doc If an ACK is received while the INVITE server transaction is in
%% the "Accepted" state, then the ACK must be passed up to the TU.
%% @end
'ACCEPTED'({request, 'ACK', Request}, TxState) ->
    ok = sip_transaction_base:pass_to_tu(Request, TxState),
    {next_state, 'ACCEPTED', TxState};

%% @doc The purpose of the "Accepted" state is to absorb retransmissions
%% of an accepted INVITE request.  Any such retransmissions are absorbed
%% entirely within the server transaction.
%% @end
'ACCEPTED'({request, 'INVITE', _Request}, TxState) ->
    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    {next_state, 'ACCEPTED', TxState};

%% @doc While in the "Accepted" state, if the TU passes a 2xx response,
%% the server transaction MUST pass the response to the transport
%% layer for transmission.
%% @end
'ACCEPTED'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 299 ->

    ok = sip_transaction_base:send_response(Response, TxState),
    {next_state, 'ACCEPTED', TxState};

%% @doc If Timer L fires while the INVITE server transaction is in the "Accepted"
%% state, the transaction  MUST transition to the "Terminated" state.
%% @end
'ACCEPTED'({timeout, _Ref, {timerL, _}}, TxState) ->
    {stop, normal, TxState}.
