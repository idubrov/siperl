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
                  % Mock transport layer calls
                  meck:new(sip_transport, [passthrough]),
                  SendRequest = fun (Conn, To, Msg) ->
                                         ?MODULE ! {tp, Conn, {request, To, Msg}},
                                         {ok, Conn}
                                end,
                  SendResponse = fun (Conn, Msg) ->
                                          ?MODULE ! {tp, Conn, {response, Msg}},
                                          {ok, Conn}
                                 end,
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
     sip_test:with_timeout([fun(_Res) -> client_invite_ok(udp) end,
                            fun(_Res) -> client_invite_ok(tcp) end,
                            fun(_Res) -> client_invite_err(udp) end,
                            fun(_Res) -> client_invite_err(tcp) end], 60)
    }.

%% Scenario tested:
%% - client INVITE transaction is created
%% - provisional response is received
%% - 2xx response is received
client_invite_ok(Transport) ->
    register(?MODULE, self()),

    Remote = sip_test:endpoint(Transport),
    Request = sip_test:invite(Transport),
    Provisional = sip_message:create_response(Request, 100, <<"Trying">>, undefined),
    Response = sip_message:create_response(Request, 200, <<"Ok">>, <<"sometag">>),

    {ok, TxRef} = sip_transaction:start_tx(client, self(), Remote, Request),

    ?assertReceive("Expect request to be sent by tx layer",
                   {tp, _Conn, {request, Remote, Request}}),

    % Emulate provisional response received by transport layer
    ?assertEqual({ok, TxRef},
                 sip_transaction:handle(undefined, Remote, Provisional)),

    ?assertReceive("Expect provisional response to be passed to TU",
    {tx, TxRef, {response, Provisional}}),

    % Emulate final 2xx response received by transport layer
    ?assertEqual({ok, TxRef},
                 sip_transaction:handle(undefined, Remote, Response)),

    ?assertReceive("Expect final response to be passed to TU",
                   {tx, TxRef, {response, Response}}),

    ?assertReceive("Expect tx to terminate immediately after receiving final response",
                   {tx, TxRef, {terminated, normal}}),

    unregister(?MODULE),
    ok.

client_invite_err(Transport)->
    register(?MODULE, self()),

    Remote = sip_test:endpoint(Transport),
    Request = sip_test:invite(Transport),
    Response = Request#sip_message{start_line = {response, 500, <<"Internal error">>}},
    ACK = sip_message:create_ack(Request, Response),

    {ok, TxRef} = sip_transaction:start_tx(client, self(), Remote, Request),

    ?assertReceive("Expect first request to be sent by tx layer",
                   {tp, _Conn, {request, Remote, Request}}),

    % Should retransmit if unreliable, should not otherwise
    case sip_transport:is_reliable(Transport) of
        false ->
            timer:sleep(500),
            ?assertReceive("Expect retransmission (in 500 ms) to be sent by tx layer",
                           {tp, _Conn, {request, Remote, Request}});

        true ->
            ?assertReceiveNot("Expect retransmission not to be sent by tx layer",
                              {tp, _Conn, _Msg})
            end,

    % Emulate response received by transport layer
    ?assertEqual({ok, TxRef},
                 sip_transaction:handle(undefined, Remote, Response)),

    ?assertReceive("Expect response to be passed to TU",
                   {tx, TxRef, {response, Response}}),

    ?assertReceive("Expect ACK to be sent by tx layer",
                   {tp, _Conn, {request, Remote, ACK}}),

    % wait 32 seconds for unreliable transport only 
    sip_transport:is_reliable(Transport) orelse timer:sleep(32000),

    ?assertReceive("Expect tx to terminate after receiving final response",
                   {tx, TxRef, {terminated, normal}}),

    unregister(?MODULE),
    ok.

-endif.