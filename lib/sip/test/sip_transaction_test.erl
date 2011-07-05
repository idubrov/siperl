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
                                         ?MODULE ! {tp, Conn, {request, To, Msg}},
                                         {ok, Conn}
                                end,
                  SendResponse = fun (Conn, Msg) ->
                                          ?MODULE ! {tp, Conn, {response, Msg}},
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
      sip_test:with_timeout([tx_test(fun client_invite_tcp_err/0),
                             tx_test(fun client_invite_udp_err/0)], 60)
      }.

% Boilerplate initialization code common for all transaction tests
tx_test(Fun) ->
    fun (_Resource) ->
             register(?MODULE, self()),
             Result = Fun(),
             unregister(?MODULE),
             Result
    end.

client_invite_udp_err() ->
    Remote = sip_test:endpoint(udp),
    Request = sip_test:invite(udp),
    Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
    ACK = sip_message:create_ack(Request, Response),

    {ok, TxRef} = sip_transaction:start_tx(client, self(), Remote, Request),

    ?assertReceive("Expect first request to be sent by tx layer",
                    {tp, _Conn, {request, Remote, Request}}),
    timer:sleep(500),
    ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                    {tp, _Conn, {request, Remote, Request}}),

    % Emulate response received by transport layer

    ?assertEqual({ok, TxRef},
                 sip_transaction:handle(undefined, Remote, Response)),

    ?assertReceive("Expect response to be passed to TU",
                    {tx, TxRef, {response, Response}}),

    ?assertReceive("Expect ACK to be sent by tx layer",
                    {tp, _Conn, {request, Remote, ACK}}),

    timer:sleep(32000),
    ?assertReceive("Expect tx to terminate in 32 seconds after receiving final response",
                    {tx, TxRef, {terminated, normal}}),
    ok.

client_invite_tcp_err() ->
    Remote = sip_test:endpoint(tcp),
    Request = sip_test:invite(tcp),
    Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
    ACK = sip_message:create_ack(Request, Response),

    {ok, TxRef} = sip_transaction:start_tx(client, self(), Remote, Request),

    ?assertReceive("Expect first request to be sent by tx layer",
                     {tp, _Conn, {request, Remote, Request}}),
    ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                        {tp, _Conn, _Msg}),

    % Emulate response received by transport layer
    ?assertEqual({ok, TxRef},
                 sip_transaction:handle(undefined, Remote, Response)),

    ?assertReceive("Expect response to be passed to TU",
                    {tx, TxRef, {response, Response}}),

    ?assertReceive("Expect ACK to be sent by tx layer",
                    {tp, _Conn, {request, Remote, ACK}}),

    ?assertReceive("Expect tx to terminate in 0 seconds after receiving final response",
                    {tx, TxRef, {terminated, normal}}),
    ok.

-endif.