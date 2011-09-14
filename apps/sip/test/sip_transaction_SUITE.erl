%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc White-box style testing for transaction layer
%%%
%%% Starts transaction layer supervisor, mocks transport layer
%%% and runs a set of functional tests to verify transactons FSMs.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_transaction_SUITE).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").
-include("sip_test.hrl").

% callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2]).

% test cases
-export([client_invite_ok/1, client_invite_err/1, client_invite_timeout_calling/1, client_invite_timeout_proceeding/1]).
-export([client_ok/1, client_timeout_trying/1, client_timeout_proceeding/1]).
-export([server_invite_ok/1, server_invite_err/1, server_invite_timeout/1]).
-export([server_ok/1, server_err/1]).
-export([server_loop/1]).


all() ->
    [{group, udp}, {group, tcp}].

groups() ->
    Tests =
        [client_invite_ok,
         client_invite_err,
         client_invite_timeout_calling,
         client_invite_timeout_proceeding,

         client_ok,
         client_timeout_trying,
         client_timeout_proceeding,

         server_invite_ok,
         server_invite_err,
         server_invite_timeout,

         server_ok,
         server_err,

         server_loop],

    [{udp, [parallel], Tests},
     {tcp, [parallel], Tests}].

init_per_suite(Config) ->
    ok = application:start(gproc),

    % start supervisor unlinked
    {ok, Pid} = sip_transaction_sup:start_link(),
    true = erlang:unlink(Pid),

    % Mock transport layer calls to intercept messages coming from transaction layer
    ok = meck:new(sip_transport, [passthrough, no_link]),
    ok = meck:expect(sip_transport, send_request,
                     fun (_To, Msg, _Opts) ->
                              TestPid = sip_test:pid_from_message(Msg),
                              TestPid ! {tp, request, Msg},
                              ok
                     end),
    ok = meck:expect(sip_transport, send_response,
                     fun (Msg) ->
                              TestPid = sip_test:pid_from_message(Msg),
                              TestPid ! {tp, response, Msg},
                              ok
                     end),
    [{transaction_sup_pid, Pid}, {mocked, [sip_transport]} | Config].

end_per_suite(Config) ->
    ok = meck:unload(?config(mocked, Config)),

    % force transaction layer supervisor to exit
    true = exit(?config(transaction_sup_pid, Config), kill),

    ok = application:stop(gproc),
    ok.

init_per_group(all, Config) -> Config;
init_per_group(udp, Config) -> [{transport, udp}, {reliable, false} | Config];
init_per_group(tcp, Config) -> [{transport, tcp}, {reliable, true} | Config].

end_per_group(_Group, _Config) -> ok.

%% @doc
%% Scenario tested:
%% - client INVITE transaction is created
%% - provisional response is received
%% - provisional response is provided to the TU
%% - 2xx response is received
%% - 2xx response is provided to the TU
%% - transaction terminates
%% @end
client_invite_ok(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case Reliable of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    {ok, TxKey} = sip_transaction:handle_response(Provisional),

    % Should not retransmit while in PROCEEDING state
    timer:sleep(1000),
    ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                      {tp, _Kind, _Msg}),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional, _UserData}),

    % Emulate final 2xx response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 200, <<"Ok">>),
    {ok, TxKey} = sip_transaction:handle_response(Response),

    ?assertReceive("Expect final response to be passed to TU",
                   {response, Response, _UserData}),

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
client_invite_err(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),

    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 500, <<"Internal error">>),
    {ok, TxKey} = sip_transaction:handle_response(Response),

    ?assertReceive("Expect response to be passed to TU",
                   {response, Response, _UserData}),

    ACK = sip_message:create_ack(ExpectedRequest, Response),
    ?assertReceive("Expect ACK to be sent by tx layer",
                   {tp, request, ACK}),

    case Reliable of
        true ->
            % no retransmissions for reliable transports
            ok;
        false ->
            % ACK should be re-transmitted, but message should not be passed to TU
            {ok, TxKey} = sip_transaction:handle_response(Response),
            ?assertReceiveNot("Expect response not to be passed to TU",
                              {response, Response, _UserData}),
            ?assertReceive("Expect ACK to be retransmitted by tx layer",
                           {tp, request, ACK})
    end,

    % Buffer any retransmissions (unreliable only)
    % Verify buffering additional response retransmissions
    case Reliable of
        true -> ok;
        false ->
            % Emulate final response retransmission received by transport layer
            {ok, TxKey} = sip_transaction:handle_response(Response),

            ?assertReceiveNot("Expect final response not to be passed to TU",
                           {response, Response, _UserData}),
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
client_invite_timeout_calling(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    % Check 408 Request Timeout is generated by client transaction
    Response = sip_message:create_response(ExpectedRequest, 408),
    ?assertReceive("Expect response to be passed to TU", {response, Response, _UserData}),

    case Reliable of
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
client_invite_timeout_proceeding(Config) ->
    Transport  = ?config(transport, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:invite(Transport),
    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    {ok, TxKey} = sip_transaction:handle_response(Provisional),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional, _UserData}),

    timer:sleep(32000),

    % Check 408 Request Timeout is generated by client transaction
    Response = sip_message:create_response(ExpectedRequest, 408),
    ?assertReceive("Expect response to be passed to TU", {response, Response, _UserData}),

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
client_ok(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),

    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case Reliable of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    {ok, TxKey} = sip_transaction:handle_response(Provisional),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional, _UserData}),

    % Should retransmit if unreliable, should not otherwise
    timer:sleep(500),
    case Reliable of
        false ->
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, request, ExpectedRequest});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Kind, _Msg})
    end,

    % Emulate provisional response retransmission received by transport layer
    {ok, TxKey} = sip_transaction:handle_response(Provisional),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional, _UserData}),

    % Emulate final 2xx response received by transport layer
    Response = sip_message:create_response(ExpectedRequest, 200, <<"Ok">>),
    {ok, TxKey} = sip_transaction:handle_response(Response),

    ?assertReceive("Expect final response to be passed to TU",
                   {response, Response, _UserData}),

    % Verify buffering additional response retransmissions
    case Reliable of
        true ->
            ok;

        false ->
            % Emulate final 2xx response retransmission received by transport layer
            {ok, TxKey} = sip_transaction:handle_response(Response),

            ?assertReceiveNot("Expect final response not to be passed to TU",
                           {response, Response, _UserData}),
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
client_timeout_trying(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    timer:sleep(32000),

    % Check 408 Request Timeout is generated by client transaction
    Response = sip_message:create_response(ExpectedRequest, 408),
    ?assertReceive("Expect response to be passed to TU", {response, Response, _UserData}),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    case Reliable of
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
client_timeout_proceeding(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    To = #sip_destination{address = {127, 0, 0, 1}, port = 5060, transport = Transport},
    Request = sip_test:request('OPTIONS', Transport),

    {ok, TxKey} = sip_transaction:start_client_tx(self(), To, Request),
    ExpectedRequest = sip_message:with_branch(TxKey#sip_tx_client.branch, Request),
    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, request, ExpectedRequest}),

    % Emulate provisional response received by transport layer
    Provisional = sip_message:create_response(ExpectedRequest, 100, <<"Trying">>),
    {ok, TxKey} = sip_transaction:handle_response(Provisional),

    ?assertReceive("Expect provisional response to be passed to TU",
                   {response, Provisional, _UserData}),

    timer:sleep(32000),

    % Check 408 Request Timeout is generated by client transaction
    Response = sip_message:create_response(ExpectedRequest, 408),
    ?assertReceive("Expect response to be passed to TU", {response, Response, _UserData}),

    ?assertReceive("Expect tx to terminate after timeout",
                   {tx, TxKey, {terminated, {timeout, _}}}),

    case Reliable of
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
server_invite_ok(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Ringing = sip_message:create_response(Request, 180, <<"Ringing">>),
    Response = sip_message:create_response(Request, 200, <<"Ok">>),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Provisional response is sent by TU
    sip_transaction:send_response(Ringing),
    ?assertReceive("Expect provisional response is sent", {tp, response, Ringing}),

    case Reliable of
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
server_invite_err(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),
    ACK = sip_message:create_ack(Request, Response),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Final response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect response is sent", {tp, response, Response}),

    % Check retransmissions handling
    case Reliable of
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
    case Reliable of
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
server_invite_timeout(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    Request = sip_test:invite(Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Final response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect response is sent", {tp, response, Response}),

    timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after timeout final response",
                   {tx, TxKey, {terminated, {timeout, _}}}),
    case Reliable of
        true -> ?assertReceiveNot("Message queue is empty", _);
        false -> ok % we have lots of message retransmissions in message queue
    end,
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
server_ok(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    Request = sip_test:request('OPTIONS', Transport),
    Trying = sip_message:create_response(Request, 100, <<"Trying">>),
    Trying2 = sip_message:create_response(Request, 100, <<"Trying Again">>),
    Response = sip_message:create_response(Request, 200, <<"Ok">>),
    Response2 = sip_message:create_response(Request, 200, <<"Another Ok">>),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % Further request retransmissions are ignored
    {ok, TxKey} = sip_transaction:handle_request(Request),

    % Provisional response is sent by TU
    sip_transaction:send_response(Trying),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying}),

    % Additional provisioning responses
    sip_transaction:send_response(Trying2),
    ?assertReceive("Expect provisional response is sent", {tp, response, Trying2}),

    case Reliable of
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

    case Reliable of
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
    Reliable orelse timer:sleep(32000),

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
server_err(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    Request = sip_test:request('OPTIONS', Transport),
    Response = sip_message:create_response(Request, 500, <<"Internal Server Error">>),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % 500 response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect 2xx response is sent", {tp, response, Response}),

    % wait for timer J to fire
    Reliable orelse timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.


%% @doc
%% Scenario tested:
%% - non-INVITE request is received (withou tag in `To:' header)
%% - request is passed to TU
%% - is_loop_detected returns true for message with same tags, but different branch
%% @end
server_loop(Config) ->
    Transport  = ?config(transport, Config),
    Reliable = ?config(reliable, Config),

    % Generate request with To: header missing the `tag' parameter
    Request =
        sip_message:replace_top_header(
          to,
          sip_headers:address(<<"Alice">>, sip_uri:parse(<<"sip:alice@atlanta.com">>), []),
          sip_test:request('OPTIONS', Transport)),
    Response = sip_message:create_response(Request, 200, <<"Ok">>),

    % Start server transaction
    {ok, TxKey} = sip_transaction:start_server_tx(self(), Request),

    % Request2 does not match the transaction, but matches criteria in 8.2.2.2
    Request2 = sip_message:with_branch(sip_idgen:generate_branch(), Request),
    true = sip_transaction:is_loop_detected(Request2),

    % Request3 has tag, so does not match 8.2.2.2
    To = sip_message:header_top_value('to', Request),
    Request3 = sip_message:replace_top_header('to', To#sip_hdr_address{params = [{tag, <<"tag">>}]}, Request2),

    false = sip_transaction:is_loop_detected(Request3),

    % 2xx response is sent by TU
    sip_transaction:send_response(Response),
    ?assertReceive("Expect 2xx response is sent", {tp, response, Response}),

    % wait for timer J to fire
    Reliable orelse timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxKey, {terminated, normal}}),

    ?assertReceiveNot("Message queue is empty", _),
    ok.
