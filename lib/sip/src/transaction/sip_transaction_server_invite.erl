%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% RFC 3261 17.2.1 INVITE Server Transaction
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_server_invite).
-extends(sip_transaction_base).

%-behaviour(gen_fsm).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transaction.hrl").
-include_lib("sip_message.hrl").

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
%% FSM callbacks
-export([init/1, handle_info/3]).
-export(['PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3, 'CONFIRMED'/2, 'CONFIRMED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
-spec init(#params{}) -> {ok, atom(), #data{}}.
init(Params) ->
    Data = ?INIT(Params),

    % notify self to send provisional response
    % (not to block initialization)
    self() ! provisional,

    % The request MUST be passed to the TU.
    Data2 = ?TU(Data#data.request, Data),
    {ok, 'PROCEEDING', Data2}.

-spec handle_info(term(), atom(), #data{}) ->
          {stop, term(), #data{}}.
%% @doc
%% Handle case when we expect response from the TU, but it goes down
%% @end
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, 'PROCEEDING', Data) ->
    {stop, {tu_down, Pid, Info}, Data};

%% @doc
%% Send provisional response.
%% @end
handle_info(provisional, _State, Data) ->
    % send provisional response
    Trying = sip_message:create_response(Data#data.request, 100, <<"Trying">>, undefined),
    Data2 = Data#data{provisional = Trying},

    Data3 = ?PROVISIONAL(Data2),
    {next_state, 'PROCEEDING', Data3};

%% @doc
%% Let the base module handle the info.
%% @end
handle_info(Info, State, Data) ->
    sip_transaction_base:handle_info(Info, State, Data).

-spec 'PROCEEDING'(term(), term(), #data{}) -> term().
%% @doc
%% If a request retransmission is received while in the "Proceeding" state, the
%% most recent provisional response that was received from the TU MUST be passed
%% to the transport layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, _From, Data) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    Data2 = ?PROVISIONAL(Data),
    {reply, ok, 'PROCEEDING', Data2};

%% @doc
%% The TU passes any number of provisional responses to the server
%% transaction.  So long as the server transaction is in the
%% "Proceeding" state, each of these MUST be passed to the transport
%% layer for transmission.
%% @end
'PROCEEDING'({response, Status, Response}, _From, Data)
  when Status >= 100, Status =< 199 ->

    Data2 = Data#data{provisional = Response},
    Data3 = ?PROVISIONAL(Data2),
    {reply, ok, 'PROCEEDING', Data3};

%% @doc
%% If, while in the "Proceeding" state, the TU passes a 2xx response to
%% the server transaction, the server transaction MUST pass this
%% response to the transport layer for transmission.  It is not
%% retransmitted by the server transaction; retransmissions of 2xx
%% responses are handled by the TU.  The server transaction MUST then
%% transition to the "Terminated" state.
%% @end
'PROCEEDING'({response, Status, Response}, _From, Data)
  when Status >= 200, Status =< 299 ->

    Data2 = Data#data{response = Response},
    Data3 = ?RESPONSE(Data2),
    {stop, normal, ok, Data3};

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
'PROCEEDING'({response, Status, Response}, _From, Data)
  when Status >= 300, Status =< 699 ->

    Data2 = Data#data{response = Response},
    Data3 = ?RESPONSE(Data2),

    % start Timer G only for unreliable transports
    IsReliable = sip_transport:is_reliable(Data3#data.connection),
    Data4 = case IsReliable of
                true -> Data3;
                false -> ?START(timerG, Data#data.t1, Data3)
            end,
    % start Timer H
    Data5 = ?START(timerH, 64 * Data#data.t1, Data4),
    {reply, ok, 'COMPLETED', Data5}.

%% @doc
%% If timer G fires, the response is passed to the transport layer once
%% more for retransmission, and timer G is set to fire in MIN(2*T1, T2)
%% seconds.
%% @end
-spec 'COMPLETED'(term(), #data{}) -> term().
'COMPLETED'({timeout, _Ref, {timerG, Interval}}, Data) ->
    Data2 = ?RESPONSE(Data),
    NewInterval = min(2 * Interval, Data2#data.t2),
    Data3 = ?START(timerG, NewInterval, Data2),
    {next_state, 'COMPLETED', Data3};

%% @doc
%% If timer H fires while in the "Completed" state, it implies that the
%% ACK was never received.  In this case, the server transaction MUST
%% transition to the "Terminated" state, and MUST indicate to the TU
%% that a transaction failure has occurred.
%% @end
'COMPLETED'({timeout, _Ref, {timerH, _Interval}}, Data) ->
    {stop, timeout, Data}.

%% @doc
%%  If an ACK is received while the server transaction is in the
%% "Completed" state, the server transaction MUST transition to the
%% "Confirmed" state.  As Timer G is ignored in this state, any
%% retransmissions of the response will cease.
%% @end
-spec 'COMPLETED'(term(), term(), #data{}) -> term().
'COMPLETED'({request, 'ACK', _Request}, _From, Data) ->
    % cancel timerG
    Data2 = ?CANCEL(timerG, Data),

    % start timer I (only for unreliable)
    case sip_transport:is_reliable(Data2#data.connection) of
        true ->
            % skip CONFIRMED state and proceed immediately to TERMINATED state
            {stop, normal, ok, Data2};
        false ->
            Data3 = ?START(timerI, Data#data.t4, Data2),
            {reply, ok, 'CONFIRMED', Data3}
    end;

%% @doc
%% Furthermore, while in the "Completed" state, if a request retransmission
%% is received, the server SHOULD pass the response to the transport for
%% retransmission.
%% @end
'COMPLETED'({request, _Method, _Request}, _From, Data) ->
    Data2 = ?RESPONSE(Data),
    {reply, ok, 'COMPLETED', Data2}.

%% @doc
%% The purpose of the "Confirmed" state is to absorb any additional ACK
%% messages that arrive, triggered from retransmissions of the final
%% response.
%% state.
%% @end
-spec 'CONFIRMED'(term(), #data{}) -> term().
'CONFIRMED'({timeout, _Ref, {timerI, _}}, Data) ->
    {stop, normal, Data}.

%% @doc
%% Absorb any additional ACK messages
%% @end
-spec 'CONFIRMED'(term(), term(), #data{}) -> term().
'CONFIRMED'({request, _Method, _Response}, _From, Data) ->
    {reply, ok, 'CONFIRMED', Data}.
