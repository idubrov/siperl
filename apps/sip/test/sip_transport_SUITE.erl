%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc White-box style testing for transport layer
%%%
%%% Starts transport layer supervisor, mocks `sip_core:lookup_core/1'
%%% to retrieve core process `Pid' from the message `Call-ID:' header.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_transport_SUITE).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").
-include("sip_test.hrl").

% callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

% test cases
-export([send_request_udp/1, send_request_udp_multicast/1]).
-export([receive_response_udp/1, receive_response_udp_wrong/1]).
-export([receive_request_udp/1, receive_request_udp2/1]).
-export([send_response_udp_maddr/1, send_response_udp_default_port/1]).
-export([receive_request_send_response_tcp/1,
         receive_request_send_response_tcp_reconnect/1,
         send_request_udp_fallback_tcp/1]).

all() ->
    [{group, udp}, {group, tcp}].

groups() ->
    [% tests using UDP socket
     {udp, [],
      [send_request_udp,
       send_request_udp_multicast,
       receive_response_udp,
       receive_response_udp_wrong,
       receive_request_udp,
       receive_request_udp2,
       send_response_udp_maddr,
       send_response_udp_default_port]},
     % tests using TCP socket
     {tcp, [], [receive_request_send_response_tcp,
                receive_request_send_response_tcp_reconnect,
                send_request_udp_fallback_tcp]}].

%% - start gproc
%% - mock sip_config module to return ports 15060 for UDP/TCP
%% - mock sip_cores to return test process when handling message
%% - start transport layer supervisor
init_per_suite(Config) ->
    ok = application:start(gproc),

    % return our test process as handling core
    ok = meck:new(sip_cores, [no_link]),
    LookupFun =
        fun (Msg) ->
                 TestPid = sip_test:pid_from_message(Msg),
                 {ok, TestPid, #sip_core_info{}}
        end,
    ok = meck:expect(sip_cores, lookup_core, LookupFun),

    % start transport layer on port 15060
    ok = meck:new(sip_config, [passthrough, no_link]),
    ok = meck:expect(sip_config, ports, fun (udp) -> [15060]; (tcp) -> [15060] end),

    {ok, Pid} = sip_transport_sup:start_link(),
    true = erlang:unlink(Pid),

    [{transport_sup_pid, Pid}, {mocked, [sip_config, sip_cores]} | Config].

end_per_suite(Config) ->
    ok = meck:unload(?config(mocked, Config)),

    % force transport layer supervisor to exit
    true = exit(?config(transport_sup_pid, Config), kill),

    ok = application:stop(gproc),
    ok.

init_per_group(_Group, Config) -> Config.
end_per_group(_Group, _Config) -> ok.

%% Open sockets that are used for sending/receiving test data
init_per_testcase(_TestCase, Config) ->
    {ok, UDP} = gen_udp:open(25060,
                             [inet, binary,
                             {active, false}]),
    {ok, TCP} = gen_tcp:listen(25060,
                               [inet, binary,
                               {active, false},
                               {packet, raw},
                               {reuseaddr, true}]),
    [{tcp, TCP}, {udp, UDP} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = gen_tcp:close(?config(tcp, Config)),
    ok = gen_udp:close(?config(udp, Config)),
    ok.

%% Tests for RFC 3261 18.1.1 Sending Requests

%% - send invite via UDP transport
%% - receive the request through the socket
send_request_udp(Config) ->
    UDP = ?config(udp, Config),

    Request = sip_test:invite(udp),
    To = #sip_destination{address = {127, 0, 0, 1}, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),

    Via = sip_message:header_top_value(via, Request),
    Via2 = Via#sip_hdr_via{host = Hostname, port = 15060},
    ExpectedRequest = sip_message:replace_top_header('via', Via2, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests
    ok = sip_transport:send_request(To, Request, []),
    {ok, {_, _, ExpectedRequestBin}} = gen_udp:recv(UDP, size(ExpectedRequestBin), ?TIMEOUT),
    ok.

%% - send invite via UDP transport (multicast destination)
%% - receive the request through the socket
send_request_udp_multicast(Config) ->
    UDP = ?config(udp, Config),

    Request = sip_test:invite(udp),
    MAddr = {239, 0, 0, 100},
    To = #sip_destination{address = MAddr, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),

    Via = sip_message:header_top_value(via, Request),
    Via2 = Via#sip_hdr_via{host = Hostname, port = 15060,
                           params = Via#sip_hdr_via.params ++ [{maddr, <<"239.0.0.100">>}, {ttl, 4}]},

    ExpectedRequest = sip_message:replace_top_header('via', Via2, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests (sending to multicast addr)
    ok = inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),

    % Send request
    ok = sip_transport:send_request(To, Request, [{ttl, 4}]),

    {ok, {_, _, ExpectedRequestBin}} = gen_udp:recv(UDP, 2000, ?TIMEOUT),

    ok = inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),
    ok.

%% - send response through the UDP socket
%% - receive the response from the transport layer
receive_response_udp(Config) ->
    UDP = ?config(udp, Config),

    Response = sip_message:create_response(sip_test:invite(udp), 200, <<"Ok">>),

    % send with updated via
    {ok, Hostname} = inet:gethostname(),
    SentBy = {Hostname, 15060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_idgen:generate_branch()}]),
    Response2 = sip_message:replace_top_header('via', Via, Response),
    ResponseBin = sip_message:to_binary(Response2),

    % RFC 3261, 18.1.2: Receiving Responses
    ok = gen_udp:send(UDP, "127.0.0.1", 15060, ResponseBin),
    receive
        {request, _Msg} ->
            ?fail("Request is not expected here");
        {response, Msg} ->
            Response2 = sip_message:parse_all_headers(Msg)
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

%% - send response with incorrect sent-by through the UDP socket
%% - check that response is NOT received
receive_response_udp_wrong(Config) ->
    UDP = ?config(udp, Config),

    Response = sip_message:create_response(sip_test:invite(udp), 200, <<"Ok">>),
    Via = sip_headers:via(udp, {"incorrect-sent-by", 35060}, [{branch, sip_idgen:generate_branch()}]),
    WrongResponse = sip_message:replace_top_header('via', Via, Response),

    % RFC 3261, 18.1.2: Receiving Responses (wrong sent-by, should be discarded)
    ok = gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(WrongResponse)),
    receive
        {request, _Msg} -> ?fail("Request is not expected here");
        {response, _Msg} -> ?fail("Response must be discarded (wrong sent-by)")
        after ?TIMEOUT -> ok
    end.

%% - send request through the UDP socket (destination is IP address)
%% - receive the request from the transport layer
receive_request_udp(Config) ->
    UDP = ?config(udp, Config),

    SentBy = {{127, 0, 0, 1}, 25060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_idgen:generate_branch()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    % RFC 3261, 18.2.1: Receiving Requests
    ok = gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request)),
    receive
        {request, Msg} ->
            Request = sip_message:parse_all_headers(Msg);

        {response, _Msg} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

%% - send request through the UDP socket (destination is hostname)
%% - receive the request from the transport layer, check that 'received' attribute is added
receive_request_udp2(Config) ->
    UDP = ?config(udp, Config),

    SentBy = {"localhost", 25060},
    Branch = sip_idgen:generate_branch(),
    Via = sip_headers:via(udp, SentBy, [{branch, Branch}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    % RFC 3261, 18.2.1: Receiving Requests, check received is added when sent-by is hostname
    ok = gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request)),

    receive
        {request, Msg} ->
            ExpectedVia = sip_headers:via(udp, {"localhost", 25060}, [{branch, Branch}, {received, {127, 0, 0, 1}}]),
            ExpectedRequest = sip_message:replace_top_header('via', ExpectedVia, Request),

            ExpectedRequest = sip_message:parse_all_headers(Msg);

        {response, _Msg2} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

%% - send response via UDP address (to multicast destination, multicast membership is not configured, no 'received')
%% - check no message is received through UDP socket
%% - configure multicast membership
%% - send response again
%% - this time the response should be received from the UDP socket
%% - drop multicast membership
%% - send response again, this time with 'received' attribute
%% - check the response is received from the UDP socket
send_response_udp_maddr(Config) ->
    UDP = ?config(udp, Config),

    MAddr = {239, 0, 0, 100},
    SentBy = {{127, 0, 0, 1}, 25060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_idgen:generate_branch()}, {'maddr', <<"239.0.0.100">>}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    ExpectedResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (to maddr)

    % Should be timeout (membership is not configured yet!)
    ok = sip_transport:send_response(Response),
    {error, timeout} = gen_udp:recv(UDP, 2000, ?TIMEOUT),

    % This time we should receive it
    ok = inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),
    ok = sip_transport:send_response(Response),
    {ok, {_, _, ExpectedResponseBin}} = gen_udp:recv(UDP, size(ExpectedResponseBin), ?TIMEOUT),
    ok = inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),

    % RFC 3261, 18.2.2: Sending Responses (to received)
    Via3 = #sip_hdr_via{host = "localhost", port = 25060, transport = udp, params = [{'received',  {127, 0, 0, 1}}]},
    Response3 = #sip_response{status = 200, reason = <<"Ok">>,
                             headers = [{'via', [Via3]}]},
    ExpectedResponseBin2 = sip_message:to_binary(Response3),

    ok = sip_transport:send_response(Response3),
    {ok, {_, _, ExpectedResponseBin2}} = gen_udp:recv(UDP, size(ExpectedResponseBin2), ?TIMEOUT),
    ok.

%% - send response via UDP address (no port is specified)
%% - check the response is received from the UDP socket opened on default port (5060)
send_response_udp_default_port(_Config) ->
    SentBy = {{127, 0, 0, 1}, undefined},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_idgen:generate_branch()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    ExpectedResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (to sent-by and default port)

    % check on default port
    {ok, DefaultUDP} = gen_udp:open(5060, [inet, binary, {active, false}]),
    ok = sip_transport:send_response(Response),
    {ok, {_, _, ExpectedResponseBin}} = gen_udp:recv(DefaultUDP, size(ExpectedResponseBin), ?TIMEOUT),
    ok = gen_udp:close(DefaultUDP),
    ok.

%% - send request via TCP connection
%% - receive request from the transport layer
%% - send response via transport layer
%% - check the response is received from same TCP connection
receive_request_send_response_tcp(_Config) ->
    SentBy = {{127, 0, 0, 1}, 25060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_idgen:generate_branch()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(tcp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    ExpectedResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (reliable protocol, same connection)
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 15060,
                                   [inet, binary, {active, false}, {packet, raw}]),

    ok = gen_tcp:send(Socket, sip_message:to_binary(Request)),
    receive
        {request, Msg} ->
            Request = sip_message:parse_all_headers(Msg);

        {response, _Msg} ->
            ?fail("Response is not expected here")
        after ?TIMEOUT ->
            ?fail("Message expected to be received by transport layer")
    end,

    % Send response
    ok = sip_transport:send_response(Response),
    {ok, ExpectedResponseBin} = gen_tcp:recv(Socket, size(ExpectedResponseBin), ?TIMEOUT),

    ok = gen_tcp:close(Socket),
    ok.

%% - send request via TCP connection
%% - receive request from the transport layer
%% - close the TCP connection
%% - send response via transport layer
%% - check that TCP connection is reopened and the response is received from it
receive_request_send_response_tcp_reconnect(Config) ->
    TCP = ?config(tcp, Config),

    SentBy = {{127, 0, 0, 1}, 25060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_idgen:generate_branch()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(tcp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    ExpectedResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (reliable protocol, server reopens connection)
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 15060,
                                   [inet, binary, {active, false}, {packet, raw}]),

    ok = gen_tcp:send(Socket, sip_message:to_binary(Request)),
    receive
        {request, Msg} ->
            Request = sip_message:parse_all_headers(Msg);

        {response, _Msg2} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end,

    % Close connection
    ok = gen_tcp:close(Socket),
    ok = timer:sleep(?TIMEOUT),
    % Send response
    ok = sip_transport:send_response(Response),

    % Server should retry by opening connection to received:sent-by-port
    {ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),
    {ok, ExpectedResponseBin} = gen_tcp:recv(RecvSocket, size(ExpectedResponseBin), ?TIMEOUT),

    ok = gen_tcp:close(RecvSocket),
    ok = gen_tcp:close(Socket),
    ok.

%% - send large request via UDP transport
%% - check that request is received via TCP connection
send_request_udp_fallback_tcp(Config) ->
    TCP = ?config(tcp, Config),

    LongBody = sip_test:generate_body(<<$A>>, 1300),
    Request = (sip_test:invite(tcp))#sip_request{body = LongBody},
    To = #sip_destination{address = {127, 0, 0, 1}, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),
    Via = sip_message:header_top_value(via, Request),
    Via2 = Via#sip_hdr_via{transport = tcp, host = Hostname, port = 15060},
    ExpectedRequest = sip_message:replace_top_header('via', Via2, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests (falling back to congestion-controlled protocol)
    ok = sip_transport:send_request(To, Request, []),
    {ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),

    {ok, ExpectedRequestBin} = gen_tcp:recv(RecvSocket, size(ExpectedRequestBin), ?TIMEOUT),
    ok = gen_tcp:close(RecvSocket),
    ok.
