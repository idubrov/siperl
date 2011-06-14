%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction layer functional tests. 
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_test).

-behaviour(sip_router).

%% Router callbacks
-export([handle/3]).
-export([send_request/3, send_response/2]).

%% Include files
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_test.hrl").


%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------

-spec handle(sip_transport:connection(), #sip_endpoint{}, #sip_message{}) -> ok.
handle(Conn, From, Msg) ->
	sip_router:handle(Conn, From, Msg).

%% route requests to self instead of transport layer
-spec send_request(sip_transport:connection() | undefined, #sip_endpoint{}, #sip_message{}) -> sip_transport:connection().
send_request(Conn, To, Msg) ->
	?MODULE ! {request, Conn, To, Msg},
	{ok, Conn}.

%% route responses to self instead of transport layer
-spec send_response(sip_transport:connection() | undefined, #sip_message{}) -> sip_transport:connection().
send_response(Conn, Msg) ->
	?MODULE ! {response, Conn, Msg},
	{ok, Conn}.

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(EUNIT).

-spec transaction_test_() -> term().
transaction_test_() ->
	% Listen on 15060
	Cfg = sip_config:from_options([{udp, [15060]}, {tcp, [15060]}, {router, sip_transaction_test}]),
	Setup = 
		fun () -> {ok, Pid} = sip_transaction_sup:start_link(Cfg),
				  {Pid} 
		end,
	Cleanup = 
		fun ({Pid}) ->
				 sip_test:shutdown_sup(Pid),
				 ok
		end,
	{foreach, Setup, Cleanup, 
	  sip_test:with_timeout([fun client_invite_tcp_err/1,
							 fun client_invite_udp_err/1], 60)
	  }.  

client_invite_udp_err({_Tx}) ->
	register(?MODULE, self()),
	To = sip_test:endpoint(udp),
	Request = sip_test:invite(udp),
	
	{ok, TxRef} = sip_transaction:start_tx(client, self(), To, Request),
	% First request
	receive
		{request, _Conn, To, Request} -> ok
	after ?TIMEOUT -> ?fail("Request expected to be sent by transaction layer")
	end,
	
	% Retransmission (in 500 ms)
	receive
		{request, _Conn2, To, Request} -> ok
	after ?TIMEOUT * 2 -> ?fail("Request expected to be retransmitted by transaction layer")
	end,
	
	Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
	handle(undefined, To, Response),
	
	receive
		{tx, TxRef, {response, Response}} ->
			ok
		
	after ?TIMEOUT -> ?fail("Should receive response from transaction layer!")
	end,
	
	% Wait for ACK
	ACK = sip_message:create_ack(Request, Response),
	receive
		{request, _Conn3, _To3, ACK} -> ok	
	after ?TIMEOUT -> ?fail("ACK should be received!")
	end,
	
	% should terminate in 32 seconds after receiving final response
	timer:sleep(32000),
	receive
		{tx, TxRef, {terminated, normal}} -> ok	
	after ?TIMEOUT -> ?fail("Transaction should terminate at that moment!")
	end,
	
	unregister(?MODULE),
	ok.

client_invite_tcp_err({_Tx}) ->
	register(?MODULE, self()),
	Dest = sip_test:endpoint(tcp),
	Request = sip_test:invite(tcp),
	
	{ok, TxRef} = sip_transaction:start_tx(client, self(), Dest, Request),
	% First request
	receive
		{request, _Conn, Dest, Request} -> ok
	after ?TIMEOUT -> ?fail("Request expected to be sent by transaction layer")
	end,
	
	% Retransmission should not be sent
	receive
		{request, _Conn2, _To2, _Msg2} -> ?fail("Retransmissions should not be sent")
	after ?TIMEOUT * 2 -> ok
	end,
	
	Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
	handle(undefined, Dest, Response),
	
	receive
		{tx, TxRef, {response, Response}} ->
			ok
	after ?TIMEOUT -> ?fail("Should receive response from transaction layer!")
	end,
	
	% Wait for ACK
	ACK = sip_message:create_ack(Request, Response),
	receive
		{request, _Conn3, _To3, ACK} -> ok	
	after ?TIMEOUT -> ?fail("ACK should be received!")
	end,
	
	% should terminate in 0 seconds after receiving final response
	receive
		{tx, TxRef, {terminated, normal}} -> ok	
	after ?TIMEOUT -> ?fail("Transaction should terminate at that moment!")
	end,
	
	unregister(?MODULE),
	ok.

-endif.