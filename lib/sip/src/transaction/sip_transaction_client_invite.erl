%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% RFC 3261 17.1.1 INVITE Client Transaction
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_client_invite).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transaction.hrl").
-include_lib("sip.hrl").

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
%% FSM callbacks
-export([init/1]).
-export(['CALLING'/2, 'CALLING'/3, 'PROCEEDING'/2, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
-spec init(#tx_state{}) -> {ok, atom(), #tx_state{}}.
init(Params) ->
    TxState = ?INIT(Params),

    % start Timer A only for unreliable transports
    IsReliable = TxState#tx_state.reliable,
    TxState2 = case IsReliable of
                true -> TxState;
                false -> ?START(timerA, ?T1, TxState)
            end,
    % timer B
    TxState3 = ?START(timerB, 64 * ?T1, TxState2),
    % send request
    TxState4 = ?REQUEST(TxState3),
    {ok, 'CALLING', TxState4}.

%% @doc
%% Handle retransmission timer (Timer A).
%% @end
-spec 'CALLING'(term(), #tx_state{}) -> term().
'CALLING'({timeout, _Ref, {timerA, Interval}}, TxState) ->
    TxState2 = ?REQUEST(TxState),
    % request is retransmitted with intervals that double after each transmission
    TxState3 = ?START(timerA, 2 * Interval, TxState2),
    {next_state, 'CALLING', TxState3};

%% @doc
%% Transaction timed out.
%% @end
'CALLING'({timeout, _Ref, {timerB, _}}, TxState) ->
    {stop, timeout, TxState}.

%% @doc
%% Handle provisional (1xx) responses. This handles both 'CALLING' and 'PROCEEDING'
%% states as they are similar.
%% @end
-spec 'CALLING'(term(), term(), #tx_state{}) -> term().
'CALLING'({response, Status, Response}, _From, TxState)
  when Status >= 100, Status =< 199 ->
    %% Provisional response, transition to PROCEEDING state.

    ?TU(Response, TxState),
    % stop retransmissions in 'PROCEEDING' state
    TxState2 = ?CANCEL(timerA, TxState),
    {reply, ok, 'PROCEEDING', TxState2};


%% @doc
%% Error response, transition to COMPLETED state.
%% @end
'CALLING'({response, Status, Response}, _From, TxState)
  when Status >= 300, Status =< 699 ->

    ?TU(Response, TxState),
    TxState2 = ?ACK(Response, TxState),

    % cancel the timers A and B
    TxState3 = ?CANCEL(timerA, TxState2),
    TxState4 = ?CANCEL(timerB, TxState3),

    % start timer D (for unreliable)
    case TxState4#tx_state.reliable of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, ok, TxState4};
        false ->
            TxState5 = ?START(timerD, 32000, TxState4),
            {reply, ok, 'COMPLETED', TxState5}
    end;


'CALLING'({response, Status, Response}, _From, TxState)
  when Status >= 200, Status =< 299 ->
    %% 2xx response, transition to the 'TERMINATED' state

    ?TU(Response, TxState),
    {stop, normal, ok, TxState}.

%% @doc
%% In 'PROCEEDING' state, act the same way as in 'CALLING' state.
%% The only difference is Timer A which is cancelled during transition
%% to the PROCEEDING state.
%% @end
-spec 'PROCEEDING'(term(), term(), #tx_state{}) -> term().
'PROCEEDING'({response, Status, Msg}, From, TxState) ->
    'CALLING'({response, Status, Msg}, From, TxState).

%% @doc
%% Transaction timed out
%% @end
-spec 'PROCEEDING'(term(), #tx_state{}) -> term().
'PROCEEDING'({timeout, _Ref, {timerB, _}}, TxState) ->
    {stop, timeout, TxState}.

%% @doc
%% In 'COMPLETED' state transaction re-sends ACK without passing the response
%% to the Transaction User
%% @end
-spec 'COMPLETED'(term(), term(), #tx_state{}) -> term().
'COMPLETED'({response, _Status, Response}, _From, TxState) ->
    TxState2 = ?ACK(Response, TxState),
    {reply, ok, 'COMPLETED', TxState2}.

%% @doc
%% When Timer D fires while in 'COMPLETED' state, transition to the 'TERMINATED'
%% state.
%% @end
-spec 'COMPLETED'(term(), #tx_state{}) -> term().
'COMPLETED'({timeout, _Ref, {timerD, _}}, TxState) ->
    {stop, normal, TxState}.
