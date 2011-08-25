%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Transaction data structures.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------

%% Types
-record(tx_state,
        {tx_key,        % Transaction key
         tx_user,        % Transaction user
         timerA,        % Timer A, RFC 3261 17.1.1.2
         timerB,        % Timer B, RFC 3261 17.1.1.2
         timerD,        % Timer D, RFC 3261 17.1.1.2
         timerE,        % Timer E, RFC 3261 17.1.2.2
         timerF,        % Timer F, RFC 3261 17.1.2.2
         timerG,        % Timer G, RFC 3261 17.2.1
         timerH,        % Timer H, RFC 3261 17.2.1
         timerI,        % Timer I, RFC 3261 17.2.1
         timerJ,        % Timer J, RFC 3261 17.2.2
         timerK,        % Timer K, RFC 3261 17.1.2.2
         request,       % Original request
         provisional,   % Most recent provisional response
         response,      % Final response
         to,            % Address to send request to
         reliable       % If request was made via reliable connection
        }).

-define(CANCEL(Timer, TxState), sip_transaction_base:cancel_timer(#tx_state.Timer, TxState)).
-define(START(Timer, Interval, TxState), sip_transaction_base:start_timer(Timer, #tx_state.Timer, Interval, TxState)).
-define(ACK(Response, TxState), sip_transaction_base:send_ack(Response, TxState)).
-define(REQUEST(TxState), sip_transaction_base:send_request(TxState#tx_state.request, TxState)).
-define(RESPONSE(TxState), sip_transaction_base:send_response(TxState#tx_state.response, TxState)).
-define(PROVISIONAL(TxState), sip_transaction_base:send_response(TxState#tx_state.provisional, TxState)).
-define(INIT(Opts), sip_transaction_base:init(Opts)).
-define(TU(Msg, TxState), sip_transaction_base:pass_to_tu(Msg, TxState)).
-define(T1, sip_config:t1()).
-define(T2, sip_config:t2()).
-define(T4, sip_config:t4()).
-define(TU_TIMEOUT, 1000). % Timeout when waiting for TU