%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction data structures.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------

%% Types
-record(data, {t1,
			   t2,
			   t4,
			   tx_key,		% Transaction key
			   tx_user,		% Transaction user
			   timerA,		% Timer A, RFC 3261 17.1.1.2
			   timerB,		% Timer B, RFC 3261 17.1.1.2
			   timerD,		% Timer D, RFC 3261 17.1.1.2
			   timerE,		% Timer E, RFC 3261 17.1.2.2
			   timerF,		% Timer F, RFC 3261 17.1.2.2
			   timerG,		% Timer G, RFC 3261 17.2.1
			   timerH,		% Timer H, RFC 3261 17.2.1
			   timerI,		% Timer I, RFC 3261 17.2.1
			   timerJ,		% Timer J, RFC 3261 17.2.2
			   timerK,		% Timer K, RFC 3261 17.1.2.2
			   request,     % Original request
			   provisional, % Most recent provisional response
			   response,    % Final response
			   to,          % Address to send request to
               connection,  % Connection request was received on
               reliable
  			  }).

% Initial transaction parameters
-record(params, {request,   % Request that initiated the transaction
                 key,       % Transaction key
                 tx_user,   % Transaction user (TU)
                 remote,    % Remote endpoint that transaction communicates with
                 to,        % Address to send request to
                 connection % Connection original request was received on 
                }).

-define(CANCEL(Timer, Data), sip_transaction_base:cancel_timer(#data.Timer, Data)).
-define(START(Timer, Interval, Data), sip_transaction_base:start_timer(Timer, #data.Timer, Interval, Data)).
-define(ACK(Response, Data), sip_transaction_base:send_ack(Response, Data)).
-define(REQUEST(Data), sip_transaction_base:send_request(Data#data.request, Data)).
-define(RESPONSE(Data), sip_transaction_base:send_response(Data#data.response, Data)).
-define(PROVISIONAL(Data), sip_transaction_base:send_response(Data#data.provisional, Data)).
-define(INIT(Opts), sip_transaction_base:init(Opts)).
-define(TU(Msg, Data), sip_transaction_base:pass_to_tu(Msg, Data)).
