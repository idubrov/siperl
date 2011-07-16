%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction layer functional tests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_test).

%% Exports
-compile(export_all).

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(EUNIT).

-spec transaction_test_() -> term().
transaction_test_() ->
    Tests = [client_invite_ok,
             client_invite_err,
             client_invite_timeout_calling,
             client_invite_timeout_proceeding,

             client_ok,
             client_timeout_trying,
             client_timeout_proceeding,

             server_invite_ok,
             server_invite_err,
             server_invite_timeout,
             server_invite_tu_down,

             server_ok,
             server_err,
             server_tu_down],
    specs(Tests).

%% @doc
%% Generates EUnit specification for given tests. Each test is given
%% as function name (atom).
%% Could be used like:
%% eunit:test(sip_transaction_test:test_([client_invite_ok]))
%% @end
-spec specs([atom()]) -> term().
specs(Funs) ->
    Transports = [tcp, udp],
    Tests = [{timeout, 60, fun () -> sip_transaction_test:Test(Transport) end} || Test <- Funs, Transport <- Transports],
    {setup, fun setup/0, fun cleanup/1, {inparallel, Tests}}.

setup() ->
    application:start(gproc),
    {ok, Pid} = sip_transaction_sup:start_link(),
    % Mock transport layer calls to intercept messages coming from transaction layer
    meck:new(sip_transport, [passthrough]),
    meck:expect(sip_transport, send_request,
                fun (To, Msg, _Opts) ->
                         TestPid = sip_test:pid_from_branch(Msg),
                         TestPid ! {tp, To, {request, Msg}},
                         ok
                end),
    meck:expect(sip_transport, send_response,
                fun (Connection, Msg) ->
                         TestPid = sip_test:pid_from_branch(Msg),
                         TestPid ! {tp, Connection, {response, Msg}},
                         ok
                end),
    {Pid}.

cleanup({Pid}) ->
    meck:unload(sip_transport),
    sip_test:shutdown_sup(Pid),
    application:stop(gproc),
    ok.

%% Generate list of test functions with single parameter (resource created by setup)
%% for every possible combination of test function and transport. Each test function
%% from the list accepts single transport parameter.
for_transports(Tests, Transports) ->
    [fun (_Res) -> Test(Transport) end || Test <- Tests, Transport <- Transports].

%% @doc
%% Scenario tested:
%% - client INVITE transaction is created
%% - provisional response is received
%% - provisional response is provided to the TU
%% - 2xx response is received
%% - 2xx response is provided to the TU
%% - transaction terminates
%% @end
client_invite_ok(Transport) ->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    Provisional = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Response = sip_message:create_response(Request, 200, <<"Ok">>, <<"sometag">>),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),

    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, _Connection, {request, Request}}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, _Connection, {request, Request}});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Connection, _Msg})
    end,

    % Emulate provisional response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    % Should not retransmit while in PROCEEDING state
    timer:sleep(1000),
    ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                      {tp, _Connection, _Msg}),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {tx, TxKey, {response, Provisional}}),

    % Emulate final 2xx response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect final response to be passed to TU",
                   {tx, TxKey, {response, Response}}),

    ?assertReceive("Expect tx to terminate immediately after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - client INVITE transaction is created
%% - response is received
%% - transaction sends ACK and provides response to TU
%% - response retransmission is received
%% - transaction retransmits ACK, but does not provide response to TU
%% - 5xx response is received
%% - transaction terminates
%% @end
client_invite_err(Transport)->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    Response = sip_message:create_response(Request, 500, <<"Internal error">>, undefined),
    ACK = sip_message:create_ack(Request, Response),
    IsReliable = sip_transport:is_reliable(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),

    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    % Emulate response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect response to be passed to TU",
                   {tx, TxKey, {response, Response}}),

    ?assertReceive("Expect ACK to be sent by tx layer",
                   {tp, _Conn, {request, ACK}}),


    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % ACK should be re-transmitted, but message should not be passed to TU
            ?assertEqual({ok, TxKey},
                         sip_transaction:handle_response(Response)),
            ?assertReceiveNot("Expect response not to be passed to TU",
                              {tx, TxKey, {response, Response}}),
            ?assertReceive("Expect ACK to be retransmitted by tx layer",
                           {tp, _Conn, {request, ACK}})
    end,

    % Buffer any retransmissions (unreliable only)
    % Verify buffering additional response retransmissions
    case IsReliable of
        true -> ok;
        false ->
            % Emulate final response retransmission received by transport layer
            ?assertEqual({ok, TxKey},
                         sip_transaction:handle_response(Response)),

            ?assertReceiveNot("Expect final response not to be passed to TU",
                           {tx, TxKey, {response, Response}}),
            ?assertReceive("Expect ACK to be retransmitted by tx layer",
                           {tp, _Conn, {request, ACK}}),
            timer:sleep(32000)
    end,

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - client INVITE transaction is created
%% - nothing happens in 32 seconds
%% - transaction terminates due to the timeout
%% @end
client_invite_timeout_calling(Transport)->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    case sip_transport:is_reliable(Transport) of
        true ->
            ?assertReceiveNot("Message queue is empty", _);

        false ->
            ok % we have lots of message retransmissions in message queue
    end,
    ok.

%% @doc
%% Scenario tested:
%% - client INVITE transaction is created
%% - provisional response is received
%% - provisional response is provided to the TU
%% - nothing happens in 32 seconds
%% - transaction terminates due to the timeout
%% @end
client_invite_timeout_proceeding(Transport)->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    Provisional = sip_message:create_response(Request, 100, <<"Trying">>, undefined),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    % Emulate provisional response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {tx, TxKey, {response, Provisional}}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - client non-INVITE transaction is created (')
%% - provisional response is received
%% - provisional response is provided to the TU
%% - 2xx response is received
%% - 2xx response is provided to the TU
%% - transaction terminates
%% @end
client_ok(Transport) ->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),
    Provisional = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Response = sip_message:create_response(Request, 200, <<"Ok">>, <<"sometag">>),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),

    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, _Conn, {request, Request}});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Conn, _Msg})
    end,

    % Emulate provisional response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {tx, TxKey, {response, Provisional}}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, _Conn, {request, Request}});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Conn, _Msg})
    end,

    % Emulate provisional response retransmission received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {tx, TxKey, {response, Provisional}}),

    % Emulate final 2xx response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect final response to be passed to TU",
                   {tx, TxKey, {response, Response}}),

    % Verify buffering additional response retransmissions
    case sip_transport:is_reliable(Transport) of
        true ->
            ok;

        false ->
            % Emulate final 2xx response retransmission received by transport layer
            ?assertEqual({ok, TxKey},
                         sip_transaction:handle_response(Response)),

            ?assertReceiveNot("Expect final response not to be passed to TU",
                           {tx, TxKey, {response, Response}}),
            timer:sleep(5000) % T4
    end,

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),


    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - client non-INVITE transaction is created
%% - nothing happens in 32 seconds
%% - transaction terminates due to the timeout
%% @end
client_timeout_trying(Transport)->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    case sip_transport:is_reliable(Transport) of
        true ->
            ?assertReceiveNot("Message queue is empty", _);

        false ->
            ok % we have lots of message retransmissions in message queue
    end,
    ok.

%% @doc
%% Scenario tested:
%% - client non-INVITE transaction is created
%% - provisional response is received
%% - provisional response is passed to TU
%% - nothing happens in 32 seconds
%% - transaction terminates due to the timeout
%% @end
client_timeout_proceeding(Transport)->
    To = #sip_destination{address = "127.0.0.1", port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),
    Provisional = sip_message:create_response(Request, 100, <<"Trying">>, undefined),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Request}}),

    % Emulate provisional response received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {tx, TxKey, {response, Provisional}}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    case sip_transport:is_reliable(Transport) of
        true ->
            ?assertReceiveNot("Message queue is empty", _);

        false ->
            ok % we have lots of message retransmissions in message queue
    end,
    ok.

%% @doc
%% Scenario tested:
%% - INVITE request is received
%% - request is passed to TU
%% - 100 Trying is sent
%% - provisional response is sent by TU
%% - provisional response is sent to the transport
%% - INVITE re-transmission is received
%% - provisional response is re-sent
%% - 2xx response is sent by TU
%% - 2xx response is sent to the transport
%% - transaction terminates
%% @end
server_invite_ok(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Ringing = sip_message:create_response(Request, 180, <<"Ringing">>, undefined),
    Response = sip_message:create_response(Request, 200, <<"Ok">>, <<"sometag">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Connection, Request),
    ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying}}),

    % Provisional response is sent by TU
    sip_transaction:send_response(TxKey, Ringing),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Ringing}}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect provisional response is re-sent", {tp, _Conn, {response, Ringing}}),
            ok
    end,

    % 2xx response is sent by TU
    sip_transaction:send_response(TxKey, Response),
    ?assertReceive("Expect 2xx response is sent", {tp, _Conn, {response, Response}}),

    ?assertReceive("Expect tx to terminate immediately after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - INVITE request is received
%% - request is passed to TU
%% - 100 Trying is sent
%% - 500 response is sent by TU
%% - 500 response is sent to the transport
%% - INVITE request is received
%% - 500 response is re-sent
%% - sleep 500 ms
%% - 500 response is re-sent
%% - ACK is received
%% - transaction terminates
%% @end
server_invite_err(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>, <<"sometag">>),
    ACK = sip_message:create_ack(Request, Response),
    IsReliable = sip_transport:is_reliable(Transport),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Connection, Request),
    ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying}}),

    % Final response is sent by TU
    sip_transaction:send_response(TxKey, Response),
    ?assertReceive("Expect response is sent", {tp, _Conn, {response, Response}}),

    % Check retransmissions handling
    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % INVITE retransmission received
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect response is sent", {tp, _Conn, {response, Response}}),

            % retransmission by timer
            timer:sleep(500),
            ?assertReceive("Expect response is sent", {tp, _Conn, {response, Response}}),
            ok
    end,

    % ACK is received
    {ok, TxKey} = sip_transaction:handle_request(ACK),

    % Verify buffering additional ACK retransmissions
    case sip_transport:is_reliable(Transport) of
        true ->
            ok;

        false ->
            % ACK is received
            {ok, TxKey} = sip_transaction:handle_request(ACK),
            timer:sleep(5000) % T4
    end,

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),
    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - INVITE request is received
%% - request is passed to TU
%% - 100 Trying is sent
%% - 500 response is sent by TU
%% - 500 response is sent to the transport
%% - transaction terminates due to timeout
%% @end
server_invite_timeout(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>, <<"sometag">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Connection, Request),
    ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying}}),

    % Final response is sent by TU
    sip_transaction:send_response(TxKey, Response),
    ?assertReceive("Expect response is sent", {tp, _Conn, {response, Response}}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout final response",
                   {tx, TxKey, {terminated, {timeout, _}}}),
    case IsReliable of
        true -> ?assertReceiveNot("Message queue is empty", _);
        false -> ok % we have lots of message retransmissions in message queue
    end,
    ok.

%% @doc
%% Scenario tested:
%% - INVITE request is received
%% - transaction is in the list
%% - TU process goes down
%% - transaction is not in the list of transactions
%% @end
server_invite_tu_down(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>, undefined),

    % Prepare TU
    TUFun =
        fun () ->
                 TxKey = receive {proceed, Ref} -> Ref end,
                 ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),

                 % exit from TU, this should force transaction to terminate
                 ok
         end,
    TU = erlang:spawn_link(TUFun),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(TU, Connection, Request),

    ?assertEqual([TxKey], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % have transaction in list
    TU ! {proceed, TxKey}, % notify TU about transaction
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying}}),

    % wait for TU to exit and transaction layer to process the 'DOWN' event
    timer:sleep(500),
    ?assertEqual([], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % do not have transaction in list
    ok.

%% @doc
%% Scenario tested:
%% - non-INVITE request is received
%% - request is passed to TU
%% - provisional response is sent by TU
%% - provisional response is sent to the transport
%% - request re-transmission is received
%% - provisional response is re-sent
%% - 2xx response is sent by TU
%% - 2xx response is sent to the transport
%% - request re-transmission is received
%% - 2xx response is re-sent
%% - transaction terminates
%% @end
server_ok(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:request('OPTIONS', Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Trying2 = sip_message:create_response(Request, 100, <<"Trying Again">>, undefined),
    Response = sip_message:create_response(Request, 200, <<"Ok">>, <<"sometag">>),
    Response2 = sip_message:create_response(Request, 200, <<"Another Ok">>, <<"sometag">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Connection, Request),
    ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),

    % Further request retransmissions are ignored
    {ok, TxKey} = sip_transaction:handle_request(Request),
    ?assertReceiveNot("Expect request is not passed to TU", {tx, TxKey, {request, Request}}),

    % Provisional response is sent by TU
    sip_transaction:send_response(TxKey, Trying),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying}}),

    % Additional provisioning responses
    sip_transaction:send_response(TxKey, Trying2),
    ?assertReceive("Expect provisional response is sent", {tp, _Conn, {response, Trying2}}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect provisional response is re-sent", {tp, _Conn, {response, Trying2}}),
            ok
    end,

    % 2xx response is sent by TU
    sip_transaction:send_response(TxKey, Response),
    ?assertReceive("Expect 2xx response is sent", {tp, _Conn, {response, Response}}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % additional responses are discarded
            sip_transaction:send_response(TxKey, Response2),

            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect final response is re-sent", {tp, _Conn, {response, Response}}),
            ok
    end,

    % wait for timer J to fire
    IsReliable orelse timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.


%% @doc
%% Scenario tested:
%% - non-INVITE request is received
%% - request is passed to TU
%% - 500 response is sent by TU
%% - 500 response is sent to the transport
%% - transaction terminates
%% @end
server_err(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:request('OPTIONS', Transport),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>, <<"sometag">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Connection, Request),
    ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),

    % 500 response is sent by TU
    sip_transaction:send_response(TxKey, Response),
    ?assertReceive("Expect 2xx response is sent", {tp, _Conn, {response, Response}}),

    % wait for timer J to fire
    IsReliable orelse timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.

%% @doc
%% Scenario tested:
%% - non-INVITE request is received
%% - transaction is in the list
%% - TU process goes down
%% - transaction is not in the list of transactions
%% @end
server_tu_down(Transport) ->
    Connection = #sip_connection{transport = tcp, connection = self()},
    Request = sip_test:request('OPTIONS', Transport),

    % Prepare TU
    TUFun =
        fun () ->
                 TxKey = receive {proceed, Ref} -> Ref end,
                 ?assertReceive("Expect request is passed to TU", {tx, TxKey, {request, Request}}),
                 % exit from TU, this should force transaction to terminate
                 ok
         end,
    TU = erlang:spawn_link(TUFun),

    % Request is received
    {ok, TxKey} = sip_transaction:start_server_tx(TU, Connection, Request),

    ?assertEqual([TxKey], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % have transaction in list
    TU ! {proceed, TxKey}, % notify TU about transaction

    % wait for TU to exit and transaction layer to process the 'DOWN' event
    timer:sleep(500),
    ?assertEqual([], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % do not have transaction in list
    ok.

-endif.