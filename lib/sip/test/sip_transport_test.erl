%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transport layer functional tests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_test).

%% Exports
-compile(export_all).

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(EUNIT).

-spec transport_test_() -> term().
transport_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{with, [
              fun send_request_udp/1,
              fun send_request_udp_fallback/1,
              fun send_request_udp_multicast/1,
              fun receive_response_udp/1,
              fun receive_response_udp_wrong/1,
              fun receive_request_udp/1,
              fun receive_request_udp2/1,
              fun send_response_tcp/1,
              fun send_response_tcp2/1,
              fun send_response_udp_maddr/1,
              fun send_response_udp_default_port/1
             ]}]}.

setup() ->
    application:start(gproc),
    % Listen on 15060
    meck:new(sip_config, [passthrough]),
    meck:expect(sip_config, ports, fun (udp) -> [15060]; (tcp) -> [15060] end),
    {ok, Pid} = sip_transport_sup:start_link(),
    {ok, UDP} = gen_udp:open(25060,
                             [inet, binary,
                             {active, false}]),
    {ok, TCP} = gen_tcp:listen(25060,
                               [inet, binary,
                               {active, false},
                               {packet, raw},
                               {reuseaddr, true}]),

    % Transport will pass request/response to transaction layer first,
    % then to the sip_core. So, override sip_transaction to always
    % return 'not_handled' and route message back to the test process
    % in mocked sip_core:handle
    meck:new(sip_core),
    Handle =
        fun (Connection, Msg) ->
                 Kind =
                     case Msg#sip_message.kind of
                         #sip_request{} -> request;
                         #sip_response{} -> response
                     end,
                 TestPid = sip_test:pid_from_branch(Msg),
                 TestPid ! {Kind, Connection, Msg},
                 {ok, undefined}
        end,
    meck:expect(sip_core, handle_request, Handle),
    meck:expect(sip_core, handle_response, Handle),

    meck:new(sip_transaction, [passthrough]),
    meck:expect(sip_transaction, handle_request, fun (_Msg) -> not_handled end),
    meck:expect(sip_transaction, handle_response, fun (_Msg) -> not_handled end),
    {Pid, UDP, TCP}.

cleanup({Pid, UDP, TCP}) ->
    gen_tcp:close(TCP),
    gen_udp:close(UDP),
    sip_test:shutdown_sup(Pid),
    meck:unload(sip_transaction),
    meck:unload(sip_core),
    meck:unload(sip_config),
    application:stop(gproc),
    ok.

%% Tests for RFC 3261 18.1.1 Sending Requests
send_request_udp({_Transport, UDP, _TCP}) ->
    Request = sip_test:invite(udp),
    To = #sip_destination{address = {127, 0, 0, 1}, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),
    SentBy = {sip_binary:any_to_binary(Hostname), 15060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    ExpectedRequest = sip_message:replace_top_header('via', Via, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests
    ok = sip_transport:send_request(To, Request, []),
    {ok, {_, _, Packet}} = gen_udp:recv(UDP, size(ExpectedRequestBin), ?TIMEOUT),
    ?assertEqual(ExpectedRequestBin, Packet).

send_request_udp_fallback({_Transport, _UDP, TCP}) ->
    LongBody = sip_test:generate_body(<<$A>>, 1300),
    Request = (sip_test:invite(tcp))#sip_message{body = LongBody},
    To = #sip_destination{address = {127, 0, 0, 1}, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),
    SentBy = {sip_binary:any_to_binary(Hostname), 15060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    ExpectedRequest = sip_message:replace_top_header('via', Via, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests (falling back to congestion-controlled protocol)
    ok = sip_transport:send_request(To, Request, []),
    {ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),

    {ok, Packet} = gen_tcp:recv(RecvSocket, size(ExpectedRequestBin), ?TIMEOUT),
    ?assertEqual(ExpectedRequestBin, Packet),
    gen_tcp:close(RecvSocket).

send_request_udp_multicast({_Transport, UDP, _TCP}) ->
    Request = sip_test:invite(udp),
    MAddr = {239, 0, 0, 100},
    To = #sip_destination{address = MAddr, port = 25060, transport = udp},

    % expect request with updated via
    {ok, Hostname} = inet:gethostname(),
    SentBy = {sip_binary:any_to_binary(Hostname), 15060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}, {maddr, <<"239.0.0.100">>}, {ttl, 4}]),
    ExpectedRequest = sip_message:replace_top_header('via', Via, Request),
    ExpectedRequestBin = sip_message:to_binary(ExpectedRequest),

    % RFC 3261, 18.1.1: Sending Requests (sending to multicast addr)
    inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),

    % Send request
    ok = sip_transport:send_request(To, Request, [{ttl, 4}]),

    {ok, {_, _, Packet}} = gen_udp:recv(UDP, 2000, ?TIMEOUT),

    ?assertEqual(ExpectedRequestBin, Packet),
    inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),
    ok.

receive_response_udp({_Transport, UDP, _TCP}) ->
    Response = sip_message:create_response(sip_test:invite(udp), 200, <<"Ok">>, undefined),

    % send with updated via
    {ok, Hostname} = inet:gethostname(),
    SentBy = {sip_binary:any_to_binary(Hostname), 15060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Response2 = sip_message:replace_top_header('via', Via, Response),
    ResponseBin = sip_message:to_binary(Response2),

    % RFC 3261, 18.1.2: Receiving Responses
    gen_udp:send(UDP, "127.0.0.1", 15060, ResponseBin),
    receive
        {request, _Conn, _Msg} ->
            ?fail("Request is not expected here");
        {response, _Conn, Msg} ->
            ?assertEqual(Response2, sip_message:parse_all_headers(Msg))
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

receive_response_udp_wrong({_Transport, UDP, _TCP}) ->
    Response = sip_message:create_response(sip_test:invite(udp), 200, <<"Ok">>, undefined),
    Via = sip_headers:via(udp, {<<"incorrect-sent-by">>, 35060}, [{branch, sip_test:branch_from_pid()}]),
    WrongResponse = sip_message:replace_top_header('via', Via, Response),

    % RFC 3261, 18.1.2: Receiving Responses (wrong sent-by, should be discarded)
    gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(WrongResponse)),
    receive
        {request, _Conn, _Msg} -> ?fail("Request is not expected here");
        {response, _Conn, _Msg} -> ?fail("Response must be discarded (wrong sent-by)")
        after ?TIMEOUT -> ok
    end.

receive_request_udp({_Transport, UDP, _TCP}) ->
    SentBy = {<<"127.0.0.1">>, 25060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    % RFC 3261, 18.2.1: Receiving Requests
    gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request)),
    receive
        {request, _Conn, Msg} ->
            ?assertEqual(Request, sip_message:parse_all_headers(Msg));

        {response, _Conn, _Msg} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

receive_request_udp2({_Transport, UDP, _TCP}) ->
    SentBy = {<<"localhost">>, 25060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    % RFC 3261, 18.2.1: Receiving Requests, check received is added when sent-by is hostname
    gen_udp:send(UDP, "127.0.0.1", 15060, sip_message:to_binary(Request)),

    receive
        {request, _Conn, Msg} ->
            ExpectedVia = sip_headers:via(udp, {<<"localhost">>, 25060}, [{branch, sip_test:branch_from_pid()},
                                                                          {received, <<"127.0.0.1">>}]),
            ExpectedRequest = sip_message:replace_top_header('via', ExpectedVia, Request),
            ?assertEqual(ExpectedRequest, sip_message:parse_all_headers(Msg));

        {response, _Conn, _Msg2} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end.

send_response_tcp({_Transport, _UDP, _TCP}) ->
    SentBy = {<<"127.0.0.1">>, 25060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(tcp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>, undefined),
    ResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (reliable protocol, same connection)
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 15060,
                                   [inet, binary, {active, false}, {packet, raw}]),

    gen_tcp:send(Socket, sip_message:to_binary(Request)),
    receive
        {request, _Conn, Msg} ->
            ?assertEqual(Request, sip_message:parse_all_headers(Msg));

        {response, _Conn, _Msg} ->
            ?fail("Response is not expected here")
        after ?TIMEOUT ->
            ?fail("Message expected to be received by transport layer")
    end,

    % Send response
    ok = sip_transport:send_response(Response),
    {ok, ActualResponse} = gen_tcp:recv(Socket, size(ResponseBin), ?TIMEOUT),
    ?assertEqual(ResponseBin, ActualResponse),

    gen_tcp:close(Socket),
    ok.

send_response_tcp2({_Transport, _UDP, TCP}) ->
    SentBy = {<<"127.0.0.1">>, 25060},
    Via = sip_headers:via(tcp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(tcp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>, undefined),
    ResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (reliable protocol, server reopens connection)
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 15060,
                                   [inet, binary, {active, false}, {packet, raw}]),

    gen_tcp:send(Socket, sip_message:to_binary(Request)),
    receive
        {request, _Conn, Msg} ->
            ?assertEqual(Request, sip_message:parse_all_headers(Msg));

        {response, _Conn2, _Msg2} -> ?fail("Response is not expected here")
        after ?TIMEOUT -> ?fail("Message expected to be received by transport layer")
    end,

    % Close connection
    gen_tcp:close(Socket),
    timer:sleep(?TIMEOUT),
    % Send response
    ok = sip_transport:send_response(Response),

    % Server should retry by opening connection to received:sent-by-port
    {ok, RecvSocket} = gen_tcp:accept(TCP, ?TIMEOUT),
    {ok, ActualResponse} = gen_tcp:recv(RecvSocket, size(ResponseBin), ?TIMEOUT),
    ?assertEqual(ResponseBin, ActualResponse),
    gen_tcp:close(RecvSocket),

    gen_tcp:close(Socket),
    ok.

send_response_udp_maddr({_Transport, UDP, _TCP}) ->
    MAddr = {239, 0, 0, 100},
    SentBy = {<<"127.0.0.1">>, 25060},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}, {'maddr', <<"239.0.0.100">>}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>, undefined),
    ResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (to maddr)

    % Should be timeout (membership is not configured yet!)
    ok = sip_transport:send_response(Response),
    {error, timeout} = gen_udp:recv(UDP, 2000, ?TIMEOUT),

    % This time we should receive it
    inet:setopts(UDP, [{add_membership, {MAddr, {0, 0, 0, 0}}}]),
    ok = sip_transport:send_response(Response),
    {ok, {_, _, Packet}} = gen_udp:recv(UDP, size(ResponseBin), ?TIMEOUT),
    ?assertEqual(ResponseBin, Packet),
    inet:setopts(UDP, [{drop_membership, {MAddr, {0, 0, 0, 0}}}]),

    % RFC 3261, 18.2.2: Sending Responses (to received)
    Via3 = #sip_hdr_via{host = <<"localhost">>, port = 25060, transport = udp, params = [{'received', <<"127.0.0.1">>}]},
    Response3 = #sip_message{kind = #sip_response{status = 200, reason = <<"Ok">>},
                             headers = [{'via', [Via3]}]},
    ResponseBin3 = sip_message:to_binary(Response3),

    ok = sip_transport:send_response(Response3),
    {ok, {_, _, Packet2}} = gen_udp:recv(UDP, size(ResponseBin3), ?TIMEOUT),

    ?assertEqual(ResponseBin3, Packet2),
    ok.

send_response_udp_default_port({_Transport, _UDP, _TCP}) ->
    SentBy = {<<"127.0.0.1">>, undefined},
    Via = sip_headers:via(udp, SentBy, [{branch, sip_test:branch_from_pid()}]),
    Request = sip_message:replace_top_header('via', Via, sip_test:invite(udp)),

    Response = sip_message:create_response(Request, 200, <<"Ok">>, undefined),
    ResponseBin = sip_message:to_binary(Response),

    % RFC 3261, 18.2.2: Sending Responses (to sent-by and default port)

    % check on default port
    {ok, DefaultUDP} = gen_udp:open(5060, [inet, binary, {active, false}]),
    ok = sip_transport:send_response(Response),
    {ok, {_, _, Packet}} = gen_udp:recv(DefaultUDP, size(ResponseBin), ?TIMEOUT),
    gen_udp:close(DefaultUDP),
    ?assertEqual(ResponseBin, Packet),
    ok.

-endif.