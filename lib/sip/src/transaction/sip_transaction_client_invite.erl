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
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
%% FSM callbacks
-export([init/1]).
-export(['CALLING'/2, 'CALLING'/3, 'PROCEEDING'/2, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
-spec init(#params{}) -> {ok, atom(), #data{}}.
init(Params) ->
    Data = ?INIT(Params),

    % start Timer A only for unreliable transports
    IsReliable = sip_transport:is_reliable(Data#data.remote#conn_key.transport),
    Data2 = case IsReliable of
                true -> Data;
                false -> ?START(timerA, Data#data.t1, Data)
            end,
    % timer B
    Data3 = ?START(timerB, 64 * Data2#data.t1, Data2),
    % send request
    Data4 = ?REQUEST(Data3),
    {ok, 'CALLING', Data4}.

%% @doc
%% Handle retransmission timer (Timer A).
%% @end
-spec 'CALLING'(term(), #data{}) -> term().
'CALLING'({timeout, _Ref, {timerA, Interval}}, Data) ->
    Data2 = ?REQUEST(Data),
    % request is retransmitted with intervals that double after each transmission
    Data3 = ?START(timerA, 2 * Interval, Data2),
    {next_state, 'CALLING', Data3};

%% @doc
%% Transaction timed out.
%% @end
'CALLING'({timeout, _Ref, {timerB, _}}, Data) ->
    {stop, timeout, Data}.

%% @doc
%% Handle provisional (1xx) responses. This handles both 'CALLING' and 'PROCEEDING'
%% states as they are similar.
%% @end
-spec 'CALLING'(term(), term(), #data{}) -> term().
'CALLING'({response, Status, Response}, _From, Data)
  when Status >= 100, Status =< 199 ->
    %% Provisional response, transition to PROCEEDING state.

    ?TU(Response, Data),
    % stop retransmissions in 'PROCEEDING' state
    Data2 = ?CANCEL(timerA, Data),
    {reply, ok, 'PROCEEDING', Data2};


%% @doc
%% Error response, transition to COMPLETED state.
%% @end
'CALLING'({response, Status, Response}, _From, Data)
  when Status >= 300, Status =< 699 ->

    ?TU(Response, Data),
    Data2 = ?ACK(Response, Data),

    % cancel the timers A and B
    Data3 = ?CANCEL(timerA, Data2),
    Data4 = ?CANCEL(timerB, Data3),

    % start timer D (for unreliable)
    case sip_transport:is_reliable(Data4#data.remote#conn_key.transport) of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, ok, Data4};
        false ->
            Data5 = ?START(timerD, 32000, Data4),
            {reply, ok, 'COMPLETED', Data5}
    end;


'CALLING'({response, Status, Response}, _From, Data)
  when Status >= 200, Status =< 299 ->
    %% 2xx response, transition to the 'TERMINATED' state

    ?TU(Response, Data),
    {stop, normal, ok, Data}.

%% @doc
%% In 'PROCEEDING' state, act the same way as in 'CALLING' state.
%% The only difference is Timer A which is cancelled during transition
%% to the PROCEEDING state.
%% @end
-spec 'PROCEEDING'(term(), term(), #data{}) -> term().
'PROCEEDING'({response, Status, Msg}, From, Data) ->
    'CALLING'({response, Status, Msg}, From, Data).

%% @doc
%% Transaction timed out
%% @end
-spec 'PROCEEDING'(term(), #data{}) -> term().
'PROCEEDING'({timeout, _Ref, {timerB, _}}, Data) ->
    {stop, timeout, Data}.

%% @doc
%% In 'COMPLETED' state transaction re-sends ACK without passing the response
%% to the Transaction User
%% @end
-spec 'COMPLETED'(term(), term(), #data{}) -> term().
'COMPLETED'({response, _Status, Response}, _From, Data) ->
    Data2 = ?ACK(Response, Data),
    {reply, ok, 'COMPLETED', Data2}.

%% @doc
%% When Timer D fires while in 'COMPLETED' state, transition to the 'TERMINATED'
%% state.
%% @end
-spec 'COMPLETED'(term(), #data{}) -> term().
'COMPLETED'({timeout, _Ref, {timerD, _}}, Data) ->
    {stop, normal, Data}.
