%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction layer functional tests. 
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_test).

%% Include files
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_test.hrl").


%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(EUNIT).

-spec transaction_test_() -> term().
transaction_test_() ->
	% Listen on 15060
	Cfg = sip_config:from_options([{udp, [15060]}, {tcp, [15060]}, {router, undefined}]),
	Setup = 
		fun () -> {ok, Pid} = sip_transaction_sup:start_link(Cfg),
				  meck:new(sip_transport, [passthrough]),
				  SendRequest = fun (Conn, To, Msg) ->
										 ?MODULE ! {transport, Conn, {request, To, Msg}},
										 {ok, Conn}
								end,
				  SendResponse = fun (Conn, Msg) ->
										  ?MODULE ! {transport, Conn, {response, Msg}},
										  {ok, Conn}
								 end,
				  % Mock transport layer calls
				  meck:expect(sip_transport, send_request, SendRequest),
				  meck:expect(sip_transport, send_response, SendResponse),
				  {Pid} 
		end,
	Cleanup = 
		fun ({Pid}) ->
				 meck:unload(sip_transport),
				 sip_test:shutdown_sup(Pid),
				 ok
		end,
	{foreach, Setup, Cleanup, 
	  sip_test:with_timeout([fun client_invite_tcp_err/1,
							 fun client_invite_udp_err/1], 60)
	  }.  


tp_expect(Msg) ->
	tp_expect(Msg, ?TIMEOUT).

tp_expect(Msg, Timeout) when is_integer(Timeout) ->
	receive
		{transport, _Conn, Msg} -> ok
	after Timeout -> ?fail("Message expected to be sent from transaction layer to transport layer")
	end.

tp_expect_not() ->
	tp_expect_not(?TIMEOUT).

tp_expect_not(Timeout) ->
	receive
		{transport, _Conn, _Msg} -> ?fail("Not expecting messages coming from transaction to transport layer!")
	after ?TIMEOUT -> ok
	end.

tp_receive(Conn, Remote, Msg) ->
	sip_router:handle(Conn, Remote, Msg).

tu_expect(TxRef, Msg) ->
	tu_expect(TxRef, Msg, ?TIMEOUT).

tu_expect(TxRef, Msg, Timeout) ->
	receive
		{tx, TxRef, Msg} ->
			ok
		
	after ?TIMEOUT -> ?fail("Should receive response from transaction layer!")
	end.

tu_send(TxRef, Msg) ->
	sip_transaction:send(TxRef, Msg).

client_invite_udp_err({_Tx}) ->
	register(?MODULE, self()),
	To = sip_test:endpoint(udp),
	Request = sip_test:invite(udp),
	
	{ok, TxRef} = sip_transaction:start_tx(client, self(), To, Request),
	
	% Expect first request to be sent by tx layer
	tp_expect({request, To, Request}),
	
	% Expect retransmission (in 500 ms) to be sent by tx layer
	tp_expect({request, To, Request}, ?TIMEOUT * 2),
	
	% Emulate response received by transport layer
	Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},	
	tp_receive(undefined, To, Response),

	% Expect response to be passed to TU
	tu_expect(TxRef, {response, Response}),
	
	% Expect ACK to be sent by tx layer
	ACK = sip_message:create_ack(Request, Response),
	tp_expect({request, To, ACK}),
	
	% Expect tx to terminate in 32 seconds after receiving final response
	timer:sleep(32000),
	tu_expect(TxRef, {terminated, normal}),
	
	unregister(?MODULE),
	ok.

client_invite_tcp_err({_Tx}) ->
	register(?MODULE, self()),
	To = sip_test:endpoint(tcp),
	Request = sip_test:invite(tcp),
	
	{ok, TxRef} = sip_transaction:start_tx(client, self(), To, Request),
	
	% Expect first request to be sent by tx layer
	tp_expect({request, To, Request}),
	
	% Expect retransmission not to be sent by tx layer
	tp_expect_not(?TIMEOUT * 2),
	
	% Emulate response received by transport layer
	Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
	tp_receive(undefined, To, Response),
	
	% Expect response to be passed to TU
	tu_expect(TxRef, {response, Response}),
	
	% Expect ACK to be sent by tx layer
	ACK = sip_message:create_ack(Request, Response),
	tp_expect({request, To, ACK}),
	
	% Expect tx to terminate in 0 seconds after receiving final response
	tu_expect(TxRef, {terminated, normal}),
	
	unregister(?MODULE),
	ok.

-endif.