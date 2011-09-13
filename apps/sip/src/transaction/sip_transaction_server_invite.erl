%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% RFC 3261 17.2.1 INVITE Server Transaction
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transaction_server_invite).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip_transaction.hrl").
-include("sip.hrl").

%% FSM callbacks (the rest are provided by `sip_transaction_base')
-export([handle_info/3]).
-export(['INIT'/2, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3, 'CONFIRMED'/2, 'CONFIRMED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
%% @doc `INIT' state is for heavy-weight initialization (sending request, starting timers)
%% @end
-spec 'INIT'(init, #tx_state{}) -> {next_state, atom(), #tx_state{}}.
'INIT'(init, TxState) ->
    % notify self to send provisional response
    % (not to block initialization)
    self() ! provisional,
    {next_state, 'PROCEEDING', TxState}.

-spec handle_info(term(), atom(), #tx_state{}) ->
          {stop, term(), #tx_state{}}.
%% @doc Send provisional response.
%% @end
handle_info(provisional, _State, TxState) ->
    % send provisional response
    Trying = sip_message:create_response(TxState#tx_state.request, 100),
    TxState2 = TxState#tx_state{provisional = Trying},

    TxState3 = ?PROVISIONAL(TxState2),
    {next_state, 'PROCEEDING', TxState3};

%% @doc Let the base module handle the info.
%% @end
handle_info(Info, State, TxState) ->
    sip_transaction_base:handle_info(Info, State, TxState).

-spec 'PROCEEDING'(term(), term(), #tx_state{}) -> term().
%% @doc
%% If a request retransmission is received while in the "Proceeding" state, the
%% most recent provisional response that was received from the TU MUST be passed
%% to the transport layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, _From, TxState) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    TxState2 = ?PROVISIONAL(TxState),
    {reply, ok, 'PROCEEDING', TxState2};

%% @doc
%% The TU passes any number of provisional responses to the server
%% transaction.  So long as the server transaction is in the
%% "Proceeding" state, each of these MUST be passed to the transport
%% layer for transmission.
%% @end
'PROCEEDING'({response, Status, Response}, _From, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Response},
    TxState3 = ?PROVISIONAL(TxState2),
    {reply, ok, 'PROCEEDING', TxState3};

%% @doc
%% If, while in the "Proceeding" state, the TU passes a 2xx response to
%% the server transaction, the server transaction MUST pass this
%% response to the transport layer for transmission.  It is not
%% retransmitted by the server transaction; retransmissions of 2xx
%% responses are handled by the TU.  The server transaction MUST then
%% transition to the "Terminated" state.
%% @end
'PROCEEDING'({response, Status, Response}, _From, TxState)
  when Status >= 200, Status =< 299 ->

    TxState2 = TxState#tx_state{response = Response},
    TxState3 = ?RESPONSE(TxState2),
    {stop, normal, ok, TxState3};

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
'PROCEEDING'({response, Status, Response}, _From, TxState)
  when Status >= 300, Status =< 699 ->

    TxState2 = TxState#tx_state{response = Response},
    TxState3 = ?RESPONSE(TxState2),

    % start Timer G only for unreliable transports
    IsReliable = TxState3#tx_state.reliable,
    TxState4 = case IsReliable of
                true -> TxState3;
                false -> ?START(timerG, ?T1, TxState3)
            end,
    % start Timer H
    TxState5 = ?START(timerH, 64 * ?T1, TxState4),
    {reply, ok, 'COMPLETED', TxState5}.

%% @doc
%% If timer G fires, the response is passed to the transport layer once
%% more for retransmission, and timer G is set to fire in MIN(2*T1, T2)
%% seconds.
%% @end
-spec 'COMPLETED'(term(), #tx_state{}) -> term().
'COMPLETED'({timeout, _Ref, {timerG, Interval}}, TxState) ->
    TxState2 = ?RESPONSE(TxState),
    NewInterval = min(2 * Interval, ?T2),
    TxState3 = ?START(timerG, NewInterval, TxState2),
    {next_state, 'COMPLETED', TxState3};

%% @doc
%% If timer H fires while in the "Completed" state, it implies that the
%% ACK was never received.  In this case, the server transaction MUST
%% transition to the "Terminated" state, and MUST indicate to the TU
%% that a transaction failure has occurred.
%% @end
'COMPLETED'({timeout, _Ref, {timerH, _Interval}}, TxState) ->
    {stop, {timeout, timerH}, TxState}.

%% @doc
%%  If an ACK is received while the server transaction is in the
%% "Completed" state, the server transaction MUST transition to the
%% "Confirmed" state.  As Timer G is ignored in this state, any
%% retransmissions of the response will cease.
%% @end
-spec 'COMPLETED'(term(), term(), #tx_state{}) -> term().
'COMPLETED'({request, 'ACK', _Request}, _From, TxState) ->
    % cancel timerG
    TxState2 = ?CANCEL(timerG, TxState),

    % start timer I (only for unreliable)
    case TxState2#tx_state.reliable of
        true ->
            % skip CONFIRMED state and proceed immediately to TERMINATED state
            {stop, normal, ok, TxState2};
        false ->
            TxState3 = ?START(timerI, ?T4, TxState2),
            {reply, ok, 'CONFIRMED', TxState3}
    end;

%% @doc
%% Furthermore, while in the "Completed" state, if a request retransmission
%% is received, the server SHOULD pass the response to the transport for
%% retransmission.
%% @end
'COMPLETED'({request, _Method, _Request}, _From, TxState) ->
    TxState2 = ?RESPONSE(TxState),
    {reply, ok, 'COMPLETED', TxState2}.

%% @doc
%% The purpose of the "Confirmed" state is to absorb any additional ACK
%% messages that arrive, triggered from retransmissions of the final
%% response.
%% state.
%% @end
-spec 'CONFIRMED'(term(), #tx_state{}) -> term().
'CONFIRMED'({timeout, _Ref, {timerI, _}}, TxState) ->
    {stop, normal, TxState}.

%% @doc
%% Absorb any additional ACK messages
%% @end
-spec 'CONFIRMED'(term(), term(), #tx_state{}) -> term().
'CONFIRMED'({request, _Method, _Response}, _From, TxState) ->
    {reply, ok, 'CONFIRMED', TxState}.
