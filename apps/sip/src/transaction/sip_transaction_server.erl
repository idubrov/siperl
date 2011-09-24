%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% RFC 3261 17.2.2 Non-INVITE Server Transaction
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3261</a>.
%%% @reference See <a href="http://tools.ietf.org/html/rfc6026">RFC 6026</a>.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transaction_server).
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
    gproc:mreg(p, l, TxState#tx_state.props),
    {next_state, 'TRYING', TxState}.

-spec 'TRYING'(term(), #tx_state{}) -> term().
%% @doc
%% Once in the "Trying" state, any further request retransmissions are
%% discarded.  A request is a retransmission if it matches the same server
%% transaction.
%% @end
'TRYING'({request, _Method, _Request}, TxState) ->
    {next_state, 'TRYING', TxState};

%% @doc
%% While in the "Trying" state, if the TU passes a provisional response
%% to the server transaction, the server transaction MUST enter the
%% "Proceeding" state.  The response MUST be passed to the transport
%% layer for transmission.
%% @end
'TRYING'({response, Status, Provisional}, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Provisional},
    ok = sip_transaction_base:send_response(Provisional, TxState2),
    {next_state, 'PROCEEDING', TxState2};

%% @doc
%% If the TU passes a final response (status codes 200-699) to the server
%% while in the "Proceeding" state, the transaction MUST enter the "Completed"
%% state, and the response MUST be passed to the transport layer for
%% transmission.
%% @end
'TRYING'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 699 ->
    % Same handling as in PROCEEDING state
    'PROCEEDING'({response, Status, Response}, TxState);

%% @doc Transaction cancellation, Section 9.2
%% Effectively does nothing for non-INVITE transactions
%% @end
'TRYING'(cancel, TxState) ->
    {next_state, 'TRYING', TxState}.


-spec 'PROCEEDING'(term(), #tx_state{}) -> term().
%% @doc
%% Any further provisional responses that are received from the TU while in
%% the "Proceeding" state MUST be passed to the transport layer for transmission.
%% @end
'PROCEEDING'({response, Status, Provisional}, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Provisional},
    ok = sip_transaction_base:send_response(Provisional, TxState2),
    {next_state, 'PROCEEDING', TxState2};

%% @doc
%% If a retransmission of the request is received while in the "Proceeding" state,
%% the most recently sent provisional response MUST be passed to the transport
%% layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, TxState) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    ok = sip_transaction_base:send_response(TxState#tx_state.provisional, TxState),
    {next_state, 'PROCEEDING', TxState};

%% @doc
%% If the TU passes a final response (status codes 200-699) to the server while in
%% the "Proceeding" state, the transaction MUST enter the "Completed" state, and
%% the response MUST be passed to the transport layer for transmission.
%%
%% When the server transaction enters the "Completed" state, it MUST set
%% Timer J to fire in 64*T1 seconds for unreliable transports, and zero
%% seconds for reliable transports.
%% @end
'PROCEEDING'({response, Status, Response}, TxState)
  when Status >= 200, Status =< 699 ->

    TxState2 = TxState#tx_state{response = Response},
    ok = sip_transaction_base:send_response(Response, TxState2),

    % start timer J (only for unreliable)
    case TxState2#tx_state.reliable of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, TxState2};
        false ->
            TxState3 = ?START(timerJ, 64 * ?T1, TxState2),
            {next_state, 'COMPLETED', TxState3}
    end;

%% @doc Transaction cancellation, Section 9.2
%% Effectively does nothing for non-INVITE transactions
%% @end
'PROCEEDING'(cancel, TxState) ->
    {next_state, 'PROCEEDING', TxState}.

-spec 'COMPLETED'(term(), #tx_state{}) -> term().
%% @doc
%% While in the "Completed" state, the server transaction MUST pass the
%% final response to the transport layer for retransmission whenever a
%% retransmission of the request is received.
%% @end
'COMPLETED'({request, _Method, _Request}, TxState) ->
    ok = sip_transaction_base:send_response(TxState#tx_state.response, TxState),
    {next_state, 'COMPLETED', TxState};

%% @doc
%% Any other final responses passed by the TU to the server transaction MUST
%% be discarded while in the "Completed" state.
%% @end
'COMPLETED'({response, Status, _Response}, TxState)
  when Status >= 200, Status =< 699 ->
    {next_state, 'COMPLETED', TxState};

%% @doc Transaction cancellation, Section 9.2
%% Effectively does nothing for non-INVITE transactions
%% @end
'COMPLETED'(cancel, TxState) ->
    {next_state, 'COMPLETED', TxState};

%% @doc
%% The server transaction remains in this state until Timer J fires, at which
%% point it MUST transition to the "Terminated" state.
%% @end
'COMPLETED'({timeout, _Ref, {timerJ, _}}, TxState) ->
    {stop, normal, TxState}.
