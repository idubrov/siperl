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
-include_lib("sip_test.hrl").

%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------
connection(Transport, Pid) ->
    Connection = sip_transport:connection("127.0.0.1", 5060, Transport),
    % Break into opaque type sip_transport:connection() for test purposes
    % Make "transport" connection process to be self, so we can mock
    % sip_transport send_* methods and route messages back to given
    % Pid.
    setelement(2, Connection, Pid).

connection(Transport) ->
    connection(Transport, self()).

invite(Transport) ->
    request('INVITE', Transport).

request(Method, Transport) ->
    Id = [case C of $< -> $_; $> -> $_; $. -> $_; _ -> C end || C <- pid_to_list(self())],
    Bin = sip_binary:any_to_binary(Id),
    Via = sip_headers:via(Transport, {<<"127.0.0.1">>, 25060}, [{branch, <<"z9hG4bK_", Bin/binary>>}]),
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
    [fun (R) -> {timeout, Timeout, fun() -> Fun(R) end} end || Fun <- Funs].

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