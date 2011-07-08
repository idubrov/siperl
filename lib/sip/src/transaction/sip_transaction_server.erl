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
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
%% FSM callbacks
-export([init/1, handle_info/3]).
-export(['TRYING'/3, 'PROCEEDING'/3, 'COMPLETED'/2, 'COMPLETED'/3]).


%%-----------------------------------------------------------------
%% FSM callbacks.
%%-----------------------------------------------------------------
-spec init({sip_config:config(), sip_transaction:tx_key(), term(), {#conn_key{}, #sip_message{}}}) ->
          {ok, atom(), #data{}}.
init(Opts) ->
    Data = ?INIT(Opts),

    % The request MUST be passed to the TU.
    Data2 = ?TU(Data#data.request, Data),
    {ok, 'TRYING', Data2}.

-spec handle_info(term(), atom(), #data{}) ->
          {stop, term(), #data{}}.
%% @doc
%% Handle case when we expect response from the TU, but it goes down
%% @end
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State, Data)
  when State =:= 'TRYING'; State =:= 'PROCEEDING' ->
    {stop, {tu_down, Pid, Info}, Data};

%% @doc
%% Let the base module handle the info.
%% @end
handle_info(Info, State, Data) ->
    sip_transaction_base:handle_info(Info, State, Data).

-spec 'TRYING'(term(), term(), #data{}) -> term().
%% @doc
%% Once in the "Trying" state, any further request retransmissions are
%% discarded.  A request is a retransmission if it matches the same server
%% transaction.
%% @end
'TRYING'({request, _Method, _Request}, _From, Data) ->
    {reply, ok, 'TRYING', Data};

%% @doc
%% While in the "Trying" state, if the TU passes a provisional response
%% to the server transaction, the server transaction MUST enter the
%% "Proceeding" state.  The response MUST be passed to the transport
%% layer for transmission.
%% @end
'TRYING'({response, Status, Response}, _From, Data)
  when Status >= 100, Status =< 199 ->

    Data2 = Data#data{provisional = Response},
    Data3 = ?PROVISIONAL(Data2),
    {reply, ok, 'PROCEEDING', Data3};


%% @doc
%% If the TU passes a final response (status codes 200-699) to the server
%% while in the "Proceeding" state, the transaction MUST enter the "Completed"
%% state, and the response MUST be passed to the transport layer for
%% transmission.
%% @end
'TRYING'({response, Status, Response}, From, Data)
  when Status >= 200, Status =< 699 ->
    % Same handling as in PROCEEDING state
    'PROCEEDING'({response, Status, Response}, From, Data).


-spec 'PROCEEDING'(term(), term(), #data{}) -> term().
%% @doc
%% Any further provisional responses that are received from the TU while in
%% the "Proceeding" state MUST be passed to the transport layer for transmission.
%% @end
'PROCEEDING'({response, Status, Response}, _From, Data)
  when Status >= 100, Status =< 199 ->

    Data2 = Data#data{provisional = Response},
    Data3 = ?PROVISIONAL(Data2),
    {reply, ok, 'PROCEEDING', Data3};

%% @doc
%% If a retransmission of the request is received while in the "Proceeding" state,
%% the most recently sent provisional response MUST be passed to the transport
%% layer for retransmission.
%% @end
'PROCEEDING'({request, _Method, _Request}, _From, Data) ->

    % Note: we do not compare the request with original one, assuming it must
    % be the same one.
    Data2 = ?PROVISIONAL(Data),
    {reply, ok, 'PROCEEDING', Data2};

%% @doc
%% If the TU passes a final response (status codes 200-699) to the server while in
%% the "Proceeding" state, the transaction MUST enter the "Completed" state, and
%% the response MUST be passed to the transport layer for transmission.
%%
%% When the server transaction enters the "Completed" state, it MUST set
%% Timer J to fire in 64*T1 seconds for unreliable transports, and zero
%% seconds for reliable transports.
%% @end
'PROCEEDING'({response, Status, Response}, _From, Data)
  when Status >= 200, Status =< 699 ->

    Data2 = Data#data{response = Response},
    Data3 = ?RESPONSE(Data2),

    % start timer J (only for unreliable)
    case sip_transport:is_reliable(Data3#data.remote#conn_key.transport) of
        true ->
            % skip COMPLETED state and proceed immediately to TERMINATED state
            {stop, normal, ok, Data3};
        false ->
            Data4 = ?START(timerJ, 64 * Data3#data.t1, Data3),
            {reply, ok, 'COMPLETED', Data4}
    end.

-spec 'COMPLETED'(term(), term(), #data{}) -> term().
%% @doc
%% While in the "Completed" state, the server transaction MUST pass the
%% final response to the transport layer for retransmission whenever a
%% retransmission of the request is received.  Any other final responses
%% passed by the TU to the server transaction MUST be discarded while in
%% the "Completed" state.  The server transaction remains in this state
%% until Timer J fires, at which point it MUST transition to the "Terminated"
%% state.
%% @end
'COMPLETED'({request, _Method, _Request}, _From, Data) ->
    Data2 = ?RESPONSE(Data),
    {reply, ok, 'COMPLETED', Data2};

%% @doc
%% Any other final responses passed by the TU to the server transaction MUST
%% be discarded while in the "Completed" state.
%% @end
'COMPLETED'({response, Status, _Response}, _From, Data)
  when Status >= 200, Status =< 699 ->
    {reply, ok, 'COMPLETED', Data}.

-spec 'COMPLETED'(term(), #data{}) -> term().
%% @doc
%% The server transaction remains in this state until Timer J fires, at which
%% point it MUST transition to the "Terminated" state.
%% @end
'COMPLETED'({timeout, _Ref, {timerJ, _}}, Data) ->
    {stop, normal, Data}.
