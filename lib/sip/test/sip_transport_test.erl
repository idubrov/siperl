%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transport layer functional tests. 
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_test).

-behaviour(sip_router).

%% Exports
-compile(export_all).

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
%% Router implementation
-spec handle(sip_transport:connection(), #sip_endpoint{}, #sip_message{}) -> ok.
handle(Conn, From, Msg) ->
	{Kind, _ , _} = Msg#sip_message.start_line,
	?MODULE ! {Kind, Conn, From, Msg},
	ok.

-spec send_request(sip_transport:connection() | undefined, #sip_endpoint{}, #sip_message{}) -> sip_transport:connection().
send_request(Conn, To, Msg) ->
	sip_router:send_request(Conn, To, Msg).

-spec send_response(sip_transport:connection() | undefined, #sip_message{}) -> sip_transport:connection().
send_response(Conn, Msg) ->
	sip_router:send_response(Conn, Msg).

%% Tests
-ifdef(EUNIT).

-spec transport_test_() -> term().
transport_test_() ->
	% Listen on 15060
	Config = [{udp, [15060]},
			  {tcp, [15060]},
			  {router, sip_transport_test}],
	Setup = 
		fun () -> {ok, Pid} = sip_transport_sup:start_link(Config),
				  {ok, UDP} = gen_udp:open(25060, 
										   [inet, binary, 
											{active, false}]),
				  {ok, TCP} = gen_tcp:listen(25060, 
											 [inet, binary, 
											  {active, false}, 
											  {packet, raw}, 
											  {reuseaddr, true}]),
				  {Pid, UDP, TCP} 
		end,
	Cleanup = 
		fun ({Pid, UDP, TCP}) ->
				 gen_tcp:close(TCP),
				 gen_udp:close(UDP),
				 sip_test:shutdown_sup(Pid),
				 ok
		end,
	{foreach, Setup, Cleanup, 
	 [{with, [
			  fun send_request/1, 
			  fun receive_response/1, 
			  fun receive_request/1, 
			  fun send_response/1
			 ]}]}.  
	
%% Tests for RFC 3261 18.1.1 Sending Requests
send_request({_Transport, UDP, TCP}) ->
	To = #sip_endpoint{transport = udp, address = "127.0.0.1", port = 25060},
	Via1 = #sip_hdr_via{},
	Via2 = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, 25060}, transport = udp},
	Request = #sip_message{start_line = {request, 'INVITE', <<"sip:127.0.0.1/test">>}, 
				 body = <<"Hello!">>, 
				 headers = [{'via', [Via1]}, {'via', [Via2]}]},
	
	{ok, Hostname} = inet:gethostname(),
	
	Via1Updated = #sip_hdr_via{sent_by = {sip_binary:any_to_binary(Hostname), 15060}, transport = udp},
	Headers = sip_headers:update_top_header('via', fun (_, _) -> [Via1Updated] end, 
											Request#sip_message.headers),
	ExpectedRequestBin = sip_message:to_binary(Request#sip_message{headers = Headers}),

	% RFC 3261, 18.1.1: Sending Requests
	send_request(undefined, To, Request),
	{ok, {_, 15060, Packet}} = gen_udp:recv(UDP, size(ExpectedRequestBin), ?TIMEOUT),	
	?assertEqual(ExpectedRequestBin, Packet),
	
	% RFC 3261, 18.1.1: Sending Requests (falling back to congestion-controlled protocol)
	LongBody = sip_test:generate_body(<<$A>>, 1300),
	LongRequest = Request#sip_message{body = LongBody},
	send_request(undefined, To, LongRequest),
	{ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),
	LongExpected = <<"INVITE sip:127.0.0.1/test SIP/2.0\r\n",
					 "Via: SIP/2.0/TCP ", (sip_binary:any_to_binary(Hostname))/binary, ":15060\r\n",
					 "Via: SIP/2.0/UDP 127.0.0.1:25060\r\n\r\n", 
					 LongBody/binary>>,
	{ok, LongPacket} = gen_tcp:recv(RecvSocket, size(LongExpected), ?TIMEOUT),
	?assertEqual(LongExpected, LongPacket),
	gen_tcp:close(RecvSocket),
	
	% RFC 3261, 18.1.1: Sending Requests (sending to multicast addr)
	MAddr = {239, 0, 0, 100},
	inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),
	MTo = To#sip_endpoint{address = MAddr, ttl = 4},
	
	% Send request
	send_request(undefined, MTo, Request),
	
	{ok, {_, 15060, MPacket}} = gen_udp:recv(UDP, 2000, ?TIMEOUT),
	?assertEqual(<<"INVITE sip:127.0.0.1/test SIP/2.0\r\n", 
				   "Via: SIP/2.0/UDP ", (sip_binary:any_to_binary(Hostname))/binary, ":15060;maddr=",
				   (sip_binary:any_to_binary(MAddr))/binary, ";ttl=4\r\n",
				   "Via: SIP/2.0/UDP 127.0.0.1:25060\r\n\r\n", 
				   "Hello!">>,
				 MPacket),
	inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),
	ok.

receive_response({_Transport, UDP, _TCP}) ->
	{ok, Hostname} = inet:gethostname(),
	
	Via1 = #sip_hdr_via{sent_by = {sip_binary:any_to_binary(Hostname), 15060}, transport = udp},
	Via2 = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, 25060}, transport = udp},
	Response = #sip_message{start_line = {response, 200, <<"Ok">>},
							headers = [{'via', [Via1]}, {'via', [Via2]}]},

	% Register to receive requests/responses
	register(?MODULE, self()),
	
	% RFC 3261, 18.1.2: Receiving Responses
	gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Response)),
	receive
		{request, _Conn, _From, _Msg} -> 
			?fail("Request is not expected here");
		{response, _Conn, _From, Msg} -> 
			?assertEqual(Response, sip_message:parse_whole(Msg))
		after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
	end,
	
	% RFC 3261, 18.1.2: Receiving Responses (wrong sent-by, should be discarded)
	WrongResponse = <<"SIP/2.0 200 Ok\r\n", 
				   "Via: SIP/2.0/UDP incorrect-sent-by:35060\r\n",
				   "Via: SIP/2.0/UDP 127.0.0.1:25060\r\n\r\n">>,
	
	gen_udp:send(UDP, "127.0.0.1", 15060, WrongResponse),
	receive
		{request, _Conn2, _From2, _Msg2} -> ?fail("Request is not expected here");
		{response, _Conn2, _From2, _Msg2} -> ?fail("Response must be discarded (wrong sent-by)")
		after ?TIMEOUT -> ok
	end,
	
	% Unregister
	unregister(?MODULE),
	ok.

receive_request({_Transport, UDP, _TCP}) ->
	Via = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, 25060}, transport = udp},
	Request = #sip_message{start_line = {request, 'INVITE', <<"sip:127.0.0.1/test">>},
						   body = <<"Hello!">>,
						   headers = [{'via', [Via]}]},
	
	% Register to receive requests/responses
	register(?MODULE, self()),
	
	% RFC 3261, 18.2.1: Receiving Requests
	gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request)),
	receive
		{request, _Conn, _From, Msg} ->
			?assertEqual(Request, sip_message:parse_whole(Msg));
		
		{response, _Conn, _From, _Msg} -> ?fail("Response is not expected here")
		after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
	end,
	
	% RFC 3261, 18.2.1: Receiving Requests, check received is added when sent-by is hostname
	Via2 = Via#sip_hdr_via{sent_by = {<<"localhost">>, 25060}},
	Request2 = Request#sip_message{headers = [{'via', [Via2]}]},
	gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request2)),
	
	receive
		{request, _Conn2, _From2, Msg2} ->
			Via3 = Via2#sip_hdr_via{params = [{'received', <<"127.0.0.1">>}]},			
			ExpectedRequest2 = Request2#sip_message{headers = [{'via', [Via3]}]},
			?assertEqual(ExpectedRequest2, Msg2);
		
		{response, _Conn2, _From2, _Msg2} -> ?fail("Response is not expected here")
		after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
	end,
	
	% Unregister
	unregister(?MODULE),
	ok.

send_response({_Transport, UDP, TCP}) ->
	Via = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, 25060}, transport = tcp},
	Headers = [{'via', [Via]}, {'content-length', 5}],
	Request = #sip_message{start_line = {request, 'INVITE', <<"sip:127.0.0.1/test">>},
						   body = <<"Hello">>,
						   headers = Headers},
	Response = #sip_message{start_line = {response, 200, <<"Ok">>},
							headers = Headers},
	ResponseBin = sip_message:to_binary(Response),
	
	% Register to receive requests/responses
	register(?MODULE, self()),
	
	% RFC 3261, 18.2.2: Sending Responses (reliable protocol, same connection)
	{ok, Socket} = gen_tcp:connect("127.0.0.1", 15060, 
								   [inet, binary, {active, false}, {packet, raw}]),
	
	gen_tcp:send(Socket, sip_message:to_binary(Request)),
	receive
		{request, Conn, _From, Msg} ->
			?assertEqual(Request, sip_message:parse_whole(Msg)),
			% Send response
			send_response(Conn, Response),
			{ok, ActualResponse} = gen_tcp:recv(Socket, size(ResponseBin), ?TIMEOUT),
			?assertEqual(ActualResponse, ResponseBin);
		
		{response, _Conn, _From, _Msg} -> 
			?fail("Response is not expected here")
		after ?TIMEOUT -> 
			?fail("Message expected to be received by transport layer")
	end,
	
	gen_tcp:close(Socket),
	
	% RFC 3261, 18.2.2: Sending Responses (reliable protocol, server reopens connection)
	{ok, Socket2} = gen_tcp:connect("127.0.0.1", 15060, 
								   [inet, binary, {active, false}, {packet, raw}]),
	
	gen_tcp:send(Socket2, sip_message:to_binary(Request)),
	receive
		{request, Conn2, _From2, Msg2} ->
			?assertEqual(Request, sip_message:parse_whole(Msg2)),
			% Close connection
			gen_tcp:close(Socket2),
			timer:sleep(?TIMEOUT),
			% Send response
			send_response(Conn2, Response),
			
			% Server should retry by opening connection to received:sent-by-port
			{ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),
			{ok, ActualResponse2} = gen_tcp:recv(RecvSocket, size(ResponseBin), ?TIMEOUT),
			?assertEqual(ActualResponse2, ResponseBin),
			gen_tcp:close(RecvSocket);
		
		{response, _Conn2, _From2, _Msg2} -> ?fail("Response is not expected here")
		after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
	end,
	
	gen_tcp:close(Socket2),
	
	% RFC 3261, 18.2.2: Sending Responses (to maddr)
	MAddr = {239, 0, 0, 100},
	Via2 = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, 25060}, transport = udp, params = [{'maddr', MAddr}]},
	Response2 = #sip_message{start_line = {response, 200, <<"Ok">>},
							 headers = [{'via', [Via2]}]},
	ResponseBin2 = sip_message:to_binary(Response2),
	
	% Should be timeout (membership is not configured yet!)
	send_response(undefined, Response2),
	{error, timeout} = gen_udp:recv(UDP, 2000, ?TIMEOUT),
	
	% This time we should receive it
	inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),	
	send_response(undefined, Response2),
	{ok, {_, 15060, Packet}} = gen_udp:recv(UDP, size(ResponseBin2), ?TIMEOUT),
	?assertEqual(ResponseBin2, Packet),
	inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),
	
	% RFC 3261, 18.2.2: Sending Responses (to received)
	Via3 = #sip_hdr_via{sent_by = {<<"localhost">>, 25060}, transport = udp, params = [{'received', <<"127.0.0.1">>}]},
	Response3 = #sip_message{start_line = {response, 200, <<"Ok">>},
							 headers = [{'via', [Via3]}]},
	ResponseBin3 = sip_message:to_binary(Response3),
	
	send_response(undefined, Response3),
	{ok, {_, 15060, Packet2}} = gen_udp:recv(UDP, size(ResponseBin3), ?TIMEOUT),
	?assertEqual(ResponseBin3, Packet2),
	
	% RFC 3261, 18.2.2: Sending Responses (to sent-by and default port)
	Via4 = #sip_hdr_via{sent_by = {<<"127.0.0.1">>, undefined}, transport = udp},
	Response4 = #sip_message{start_line = {response, 200, <<"Ok">>},
							 headers = [{'via', [Via4]}]},
	ResponseBin4 = sip_message:to_binary(Response4),
	
	% check on default port
	{ok, DefaultUDP} = gen_udp:open(5060, [inet, binary, {active, false}]),
	send_response(undefined, Response4),
	{ok, {_, 15060, Packet3}} = gen_udp:recv(DefaultUDP, size(ResponseBin4), ?TIMEOUT),
	gen_udp:close(DefaultUDP),
	?assertEqual(ResponseBin4, Packet3),
	
	% Unregister
	unregister(?MODULE),
	ok.

-endif.