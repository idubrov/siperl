%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Transaction data structures.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------

%% Types

%% Transaction state record. Used both for client and server transactions.
-record(tx_state,
        {tx_key             :: sip_tx_key(),        % Transaction key
         tx_user            :: pid(),               % Transaction user
         timerA             :: reference(),         % Timer A, RFC 3261 17.1.1.2
         timerB             :: reference(),         % Timer B, RFC 3261 17.1.1.2
         timerD             :: reference(),         % Timer D, RFC 3261 17.1.1.2
         timerE             :: reference(),         % Timer E, RFC 3261 17.1.2.2
         timerF             :: reference(),         % Timer F, RFC 3261 17.1.2.2
         timerG             :: reference(),         % Timer G, RFC 3261 17.2.1
         timerH             :: reference(),         % Timer H, RFC 3261 17.2.1
         timerI             :: reference(),         % Timer I, RFC 3261 17.2.1
         timerJ             :: reference(),         % Timer J, RFC 3261 17.2.2
         timerK             :: reference(),         % Timer K, RFC 3261 17.1.2.2
         timerL             :: reference(),
         timerM             :: reference(),         % Timer M, RFC 6026 8.4
         request            :: #sip_request{},      % Original request
         provisional        :: #sip_response{},     % Most recent provisional response
         response           :: #sip_response{},     % Final response
         destination        :: #sip_destination{},  % Address to send request to
         reliable           :: boolean(),           % If request was made via reliable connection
         options = []       :: [{atom(), term()}],  % Options
         props = []         :: [{term(), term()}]   % Any gproc properties to register
        }).

-define(CANCEL(Timer, TxState), sip_transaction_base:cancel_timer(#tx_state.Timer, TxState)).
-define(START(Timer, Interval, TxState), sip_transaction_base:start_timer(Timer, #tx_state.Timer, Interval, TxState)).
-define(T1, sip_config:t1()).
-define(T2, sip_config:t2()).
-define(T4, sip_config:t4()).
