%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc White-box style testing for transaction layer
%%%
%%% Starts transaction layer supervisor, mocks transport layer
%%% and runs a set of functional tests to verify transactons FSMs.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_transaction_tests).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec transaction_test_() -> term().
transaction_test_() ->
    Tests = [fun client_invite_ok/1,
             fun client_invite_err/1,
             fun client_invite_timeout_calling/1,
             fun client_invite_timeout_proceeding/1,

             fun client_ok/1,
             fun client_timeout_trying/1,
             fun client_timeout_proceeding/1,

             fun server_invite_ok/1,
             fun server_invite_err/1,
             fun server_invite_timeout/1,
             fun server_invite_tu_down/1,

             fun server_ok/1,
             fun server_err/1,
             fun server_tu_down/1
            ],
    Transports = [tcp, udp],
    Specs = [{timeout, 60, fun () -> Test(Transport) end} || Test <- Tests, Transport <- Transports],
    {setup, fun setup/0, fun cleanup/1, {inparallel, Specs}}.

setup() ->
    application:start(gproc),
    {ok, Pid} = sip_transaction_sup:start_link(),
    % Mock transport layer calls to intercept messages coming from transaction layer
    meck:new(sip_transport, [passthrough]),
    meck:expect(sip_transport, send_request,
                fun (_To, Msg, _Opts) ->
                         TestPid = sip_test:pid_from_message(Msg),
                         TestPid ! {tp, request, Msg},
                         ok
                end),
    meck:expect(sip_transport, send_response,
                fun (Msg) ->
                         TestPid = sip_test:pid_from_message(Msg),
                         TestPid ! {tp, response, Msg},
                         ok
                end),
    {Pid}.

cleanup({Pid}) ->
    meck:unload(sip_transport),
    sip_test:shutdown_sup(Pid),
    application:stop(gproc),
    ok.

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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    % Should not retransmit while in PROCEEDING state
    timer:sleep(1000),
    ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                      {tp, _Kind, _Msg}),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional}),

    % Emulate final 2xx response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 200, <<"Ok">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect final response to be passed to TU",
                   {response, Response}),

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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    IsReliable = sip_transport:is_reliable(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),

    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 500, <<"Internal error">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect response to be passed to TU",
                   {response, Response}),

    ACK = sip_message:create_ack(ExpectedRequest, Response),
    ?assertReceive("Expect ACK to be sent by tx layer",
                   {tp, request, ACK}),


    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % ACK should be re-transmitted, but message should not be passed to TU
            ?assertEqual({ok, TxKey},
                         sip_transaction:handle_response(Response)),
            ?assertReceiveNot("Expect response not to be passed to TU",
                              {response, Response}),
            ?assertReceive("Expect ACK to be retransmitted by tx layer",
                           {tp, request, ACK})
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
                           {response, Response}),
            ?assertReceive("Expect ACK to be retransmitted by tx layer",
                           {tp, request, ACK}),
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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional}),

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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),

    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case sip_transport:is_reliable(Transport) of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response retransmission received by transport layer
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional}),

    % Emulate final 2xx response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 200, <<"Ok">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Response)),

    ?assertReceive("Expect final response to be passed to TU",
                   {response, Response}),

    % Verify buffering additional response retransmissions
    case sip_transport:is_reliable(Transport) of
        true ->
            ok;

        false ->
            % Emulate final 2xx response retransmission received by transport layer
            ?assertEqual({ok, TxKey},
                         sip_transaction:handle_response(Response)),

            ?assertReceiveNot("Expect final response not to be passed to TU",
                           {response, Response}),
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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

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
    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    ?assertEqual({ok, TxKey},
                 sip_transaction:handle_response(Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional}),

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
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Ringing = sip_message:create_response(Request, 180, <<"Ringing">>),
    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Provisional response is sent by TU
    sip_transaction:send_response(Ringing),
    ?assertReceive("Expect provisional response is sent", {tp, response, Ringing}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect provisional response is re-sent", {tp, response, Ringing}),
            ok
    end,

    % 2xx response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect 2xx response is sent", {tp, response, Response}),

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
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),
    ACK = sip_message:create_ack(Request, Response),
    IsReliable = sip_transport:is_reliable(Transport),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Final response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect response is sent", {tp, response, Response}),

    % Check retransmissions handling
    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % INVITE retransmission received
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect response is sent", {tp, response, Response}),

            % retransmission by timer
            timer:sleep(500),
            ?assertReceive("Expect response is sent", {tp, response, Response}),
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
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Final response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect response is sent", {tp, response, Response}),

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
    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),

    % Prepare TU
    TUFun =
        fun () ->
                 receive {proceed, Ref} -> Ref end,
                 % exit from TU, this should force transaction to terminate
                 ok
         end,
    TU = erlang:spawn_link(TUFun),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(TU, Request),

    ?assertEqual([TxKey], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % have transaction in list
    TU ! {proceed, TxKey}, % notify TU about transaction
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

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
    Request = sip_test:request('OPTIONS', Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Trying2 = sip_message:create_response(Request, 100, <<"Trying Again">>),
    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    Response2 = sip_message:create_response(Request, 200, <<"Another Ok">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % Further request retransmissions are ignored
    {ok, TxKey} = sip_transaction:handle_request(Request),
    ?assertReceiveNot("Expect request is not passed to TU", {request, Request}),

    % Provisional response is sent by TU
    sip_transaction:send_response(Trying),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Additional provisioning responses
    sip_transaction:send_response(Trying2),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying2}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect provisional response is re-sent", {tp, response, Trying2}),
            ok
    end,

    % 2xx response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect 2xx response is sent", {tp, response, Response}),

    case IsReliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % additional responses are discarded
            sip_transaction:send_response(Response2),

            {ok, TxKey} = sip_transaction:handle_request(Request),
            ?assertReceive("Expect final response is re-sent", {tp, response, Response}),
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
    Request = sip_test:request('OPTIONS', Transport),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),
    IsReliable = sip_transport:is_reliable(Transport),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % 500 response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect 2xx response is sent", {tp, response, Response}),

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
    Request = sip_test:request('OPTIONS', Transport),

    % Prepare TU
    TUFun =
        fun () ->
                 receive {proceed, Ref} -> Ref end,
                 % exit from TU, this should force transaction to terminate
                 ok
         end,
    TU = erlang:spawn_link(TUFun),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(TU, Request),

    ?assertEqual([TxKey], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % have transaction in list
    TU ! {proceed, TxKey}, % notify TU about transaction

    % wait for TU to exit and transaction layer to process the 'DOWN' event
    timer:sleep(500),
    ?assertEqual([], [T || T <- sip_transaction:list_tx(), T =:= TxKey]), % do not have transaction in list
    ok.

-endif.