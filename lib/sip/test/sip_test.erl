%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Functions to create test requests/responses.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_test).

%% Exports
-compile(export_all).

%% Include files
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_test.hrl").

%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------
endpoint(Transport) ->
    #sip_endpoint{transport = Transport, address = "127.0.0.1", port = 5080}.

invite(Transport) ->
    request('INVITE', Transport).

request(Method, Transport) ->
    Via = sip_headers:via(Transport, {<<"127.0.0.1">>, 25060}, [{branch, <<"branch_id">>}]),
    CSeq = sip_headers:cseq(232908, Method),
    From = sip_headers:from(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
    To = sip_headers:to(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{'tag', <<"839408234">>}]),

    Headers = [CSeq, Via, From, To],
    Msg = #sip_message{start_line = sip_message:request(Method, <<"sip:127.0.0.1/test">>),
                       body = <<"Hello!">>,
                       headers = Headers},
    Msg.

generate_body(Repeat, Count) ->
    generate_body(<<>>, Repeat, Count).

generate_body(Bin, _Repeat, 0) ->
    Bin;

generate_body(Bin, Repeat, Count) ->
    generate_body(<<Bin/binary, Repeat/binary>>, Repeat, Count - 1).

%% @doc
%% Generate list of tests with timeouts for {foreach, ...} EUnit construct.
%% @end
with_timeout(Funs, Timeout) ->
    lists:map(fun (Fun) ->
                       fun (R) -> {timeout, Timeout, fun() -> Fun(R) end} end
              end, Funs).

%% @doc
%% Shutdown given linked supervisor.
%% @end
shutdown_sup(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, shutdown),
    receive
        {'EXIT', Pid, shutdown} -> ok
    after ?TIMEOUT -> ?fail("Should receive 'shutdown'")
    end,
    process_flag(trap_exit, false),
    ok.