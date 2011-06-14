%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% RFC 3261 17.1.2 Non-INVITE Client Transaction 
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_client).
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
-export(['TRYING'/2, 'TRYING'/3, 'PROCEEDING'/2, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3]).

%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
-spec init({sip_config:config(), sip_transaction:tx_key(), term(), {#sip_endpoint{}, #sip_message{}}}) ->
		  {ok, atom(), #data{}}.
init(Opts) ->
	Data = ?INIT(Opts),
	
	% start Timer E only for unreliable transports
	IsReliable = sip_transport:is_reliable(Data#data.remote#sip_endpoint.transport),
	Data2 = case IsReliable of
				true -> Data;
				false -> ?START(timerE, Data#data.t1, Data)
			end,
	% timer F
	Data3 = ?START(timerF, 64 * Data#data.t1, Data2),
	% send request
	Data4 = ?REQUEST(Data3),
	{ok, 'TRYING', Data4}.

%% @doc
%% Handle retransmission timer (Timer E).
%% @end
-spec 'TRYING'(term(), #data{}) -> term().
'TRYING'({timeout, _Ref, {timerE, Interval}}, Data) ->
	Data2 = ?REQUEST(Data),
	% request is retransmitted with intervals that double after each transmission
	NewInterval = min(2 * Interval, Data2#data.t2),
	Data3 = ?START(timerE, NewInterval, Data2),
	{next_state, 'TRYING', Data3};

%% @doc
%% Transaction timed out.
%% @end
'TRYING'({timeout, _Ref, {timerF, _}}, Data) ->
	{stop, timeout, Data}.

%% @doc
%% Handle provisional (1xx) responses. This handles both 'TRYING' and 'PROCEEDING'
%% states as they are similar.
%% @end
-spec 'TRYING'(term(), term(), #data{}) -> term().
'TRYING'({response, Status, Response}, _From, Data) 
  when Status >= 100, Status =< 199 ->
	%% Provisional response, transition to PROCEEDING state.
	
	?TU(Response, Data),	
	{reply, ok, 'PROCEEDING', Data};

'TRYING'({response, Status, Response}, _From, Data)
  when Status >= 200, Status =< 699 ->
	%% Final response, transition to COMPLETED state.
	
	?TU(Response, Data),	
	% start timer K
	Timeout = case sip_transport:is_reliable(Data#data.remote#sip_endpoint.transport) of
				  true -> 0;
				  false -> Data#data.t4
			  end,
	Data2 = ?START(timerK, Timeout, Data),
	
	% cancel the timers E and F
	Data3 = ?CANCEL(timerE, Data2),
	Data4 = ?CANCEL(timerF, Data3),
	{reply, ok, 'COMPLETED', Data4}.

%% @doc
%% In 'PROCEEDING' state, act the same way as in 'TRYING' state.
%% @end
-spec 'PROCEEDING'(term(), term(), #data{}) -> term().
'PROCEEDING'({response, Status, Msg}, From, Data) ->
	'TRYING'({response, Status, Msg}, From, Data).

-spec 'PROCEEDING'(term(), #data{}) -> term().
'PROCEEDING'({timeout, _Ref, {timerE, _Interval}}, Data) ->
	Data2 = ?REQUEST(Data),
	Data3 = ?START(timerE, Data2#data.t2, Data2),
	{next_state, 'PROCEEDING', Data3};

%% @doc
%% Transaction timed out.
%% @end
'PROCEEDING'({timeout, _Ref, {timerF, _}}, Data) ->
	{stop, timeout, Data}.

%% @doc
%% Buffer additional response retransmissions.
%% @end
-spec 'COMPLETED'(term(), term(), #data{}) -> term().
'COMPLETED'({response, _Status, _Response}, _From, Data) ->
	{reply, ok, 'COMPLETED', Data}.

%% @doc
%% When Timer K fires while in 'COMPLETED' state, transition to the 'TERMINATED'
%% state.
%% @end
-spec 'COMPLETED'(term(), #data{}) -> term().
'COMPLETED'({timeout, _Ref, {timerK, _}}, Data) ->
	{stop, normal, Data}.
