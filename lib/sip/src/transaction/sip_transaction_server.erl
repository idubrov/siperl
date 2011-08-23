%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% RFC 3261 17.2.2 Non-INVITE Server Transaction
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_server).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transaction.hrl").
-include_lib("sip.hrl").

%% FSM callbacks
-export([handle_info/3]).
-export(['INIT'/2, 'TRYING'/3, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
%% @doc `INIT' state is for heavy-weight initialization (sending request, starting timers)
%% @end
-spec 'INIT'(init, #tx_state{}) -> {next_state, atom(), #tx_state{}}.
'INIT'(init, TxState) ->
    {next_state, 'TRYING', TxState}.

-spec handle_info(term(), atom(), #tx_state{}) ->
          {stop, term(), #tx_state{}}.
%% @doc
%% Handle case when we expect response from the TU, but it goes down
%% @end
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State, TxState)
  when State =:= 'TRYING'; State =:= 'PROCEEDING' ->
    {stop, {tu_down, TxState#tx_state.tx_user}, TxState};

%% @doc
%% Let the base module handle the info.
%% @end
handle_info(Info, State, TxState) ->
    sip_transaction_base:handle_info(Info, State, TxState).

-spec 'TRYING'(term(), term(), #tx_state{}) -> term().
%% @doc
%% Once in the "Trying" state, any further request retransmissions are
%% discarded.  A request is a retransmission if it matches the same server
%% transaction.
%% @end
'TRYING'({request, _Method, _Request}, _From, TxState) ->
    {reply, ok, 'TRYING', TxState};

%% @doc
%% While in the "Trying" state, if the TU passes a provisional response
%% to the server transaction, the server transaction MUST enter the
%% "Proceeding" state.  The response MUST be passed to the transport
%% layer for transmission.
%% @end
'TRYING'({response, Status, Response}, _From, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Response},
    TxState3 = ?PROVISIONAL(TxState2),
    {reply, ok, 'PROCEEDING', TxState3};


%% @doc
%% If the TU passes a final response (status codes 200-699) to the server
%% while in the "Proceeding" state, the transaction MUST enter the "Completed"
%% state, and the response MUST be passed to the transport layer for
%% transmission.
%% @end
'TRYING'({response, Status, Response}, From, TxState)
  when Status >= 200, Status =< 699 ->
    % Same handling as in PROCEEDING state
    'PROCEEDING'({response, Status, Response}, From, TxState).


-spec 'PROCEEDING'(term(), term(), #tx_state{}) -> term().
%% @doc
%% Any further provisional responses that are received from the TU while in
%% the "Proceeding" state MUST be passed to the transport layer for transmission.
%% @end
'PROCEEDING'({response, Status, Response}, _From, TxState)
  when Status >= 100, Status =< 199 ->

    TxState2 = TxState#tx_state{provisional = Response},
    TxState3 = ?PROVISIONAL(TxState2),
    {reply, ok, 'PROCEEDING', TxState3};

%% @doc
%% If a retransmission of the request is received while in the "Proceeding" state,
%% the most recently sent provisional response MUST be passed to the transport
%% layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, _From, TxState) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    TxState2 = ?PROVISIONAL(TxState),
    {reply, ok, 'PROCEEDING', TxState2};

%% @doc
%% If the TU passes a final response (status codes 200-699) to the server while in
%% the "Proceeding" state, the transaction MUST enter the "Completed" state, and
%% the response MUST be passed to the transport layer for transmission.
%%
%% When the server transaction enters the "Completed" state, it MUST set
%% Timer J to fire in 64*T1 seconds for unreliable transports, and zero
%% seconds for reliable transports.
%% @end
'PROCEEDING'({response, Status, Response}, _From, TxState)
  when Status >= 200, Status =< 699 ->

    TxState2 = TxState#tx_state{response = Response},
    TxState3 = ?RESPONSE(TxState2),

    % start timer J (only for unreliable)
    case TxState3#tx_state.reliable of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, ok, TxState3};
        false ->
            TxState4 = ?START(timerJ, 64 * ?T1, TxState3),
            {reply, ok, 'COMPLETED', TxState4}
    end.

-spec 'COMPLETED'(term(), term(), #tx_state{}) -> term().
%% @doc
%% While in the "Completed" state, the server transaction MUST pass the
%% final response to the transport layer for retransmission whenever a
%% retransmission of the request is received.  Any other final responses
%% passed by the TU to the server transaction MUST be discarded while in
%% the "Completed" state.  The server transaction remains in this state
%% until Timer J fires, at which point it MUST transition to the "Terminated"
%% state.
%% @end
'COMPLETED'({request, _Method, _Request}, _From, TxState) ->
    TxState2 = ?RESPONSE(TxState),
    {reply, ok, 'COMPLETED', TxState2};

%% @doc
%% Any other final responses passed by the TU to the server transaction MUST
%% be discarded while in the "Completed" state.
%% @end
'COMPLETED'({response, Status, _Response}, _From, TxState)
  when Status >= 200, Status =< 699 ->
    {reply, ok, 'COMPLETED', TxState}.

-spec 'COMPLETED'(term(), #tx_state{}) -> term().
%% @doc
%% The server transaction remains in this state until Timer J fires, at which
%% point it MUST transition to the "Terminated" state.
%% @end
'COMPLETED'({timeout, _Ref, {timerJ, _}}, TxState) ->
    {stop, normal, TxState}.
