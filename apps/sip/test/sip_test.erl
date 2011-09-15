%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Functions to create test requests/responses.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_test).

%% Exports
-compile(export_all).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------
invite(Transport) ->
    request('INVITE', Transport).

%% @doc Generate test request with given `Method'. Sets `Transport' on top via.
%% @end
request(Method, Transport) ->
    Headers = [{cseq, sip_headers:cseq(232908, Method)},
               {via, [sip_headers:via(Transport, {{127, 0, 0, 1}, 25060}, [{branch, sip_idgen:generate_branch()}])]},
               {via, [sip_headers:via(udp, {{127, 0, 0, 1}, 5060}, [{branch, <<?MAGIC_COOKIE, $_, "kjshdyff">>}])]},
               {from, sip_headers:address(<<"Bob">>, sip_uri:parse(<<"sip:bob@biloxi.com">>), [{tag, <<"1928301774">>}])},
               {contact, [sip_headers:address(<<"Bob">>, sip_uri:parse(<<"sip:bob@biloxi.com">>), [])]},
               {to, sip_headers:address(<<"Alice">>, sip_uri:parse(<<"sip:alice@atlanta.com">>), [{tag, <<"89182321">>}])},
               {'call-id', list_to_binary(pid_to_list(self()))}, % encode PID in header for testing purposes
               {'max-forwards', 70},
               {'content-length', 6}],
    Msg = #sip_request{method = Method,
                       uri = sip_uri:parse(<<"sip:test@127.0.0.1">>),
                       body = <<"Hello!">>,
                       headers = Headers},
    Msg.

%% Extract test process pid from the top branch
pid_from_message(Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    PidBinary = sip_message:header_top_value('call-id', Msg),
    list_to_pid(binary_to_list(PidBinary)).

%% @doc
%% Generate binary by repeating given binary.
%% @end
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

%% @doc Shutdown process
%% @end
shutdown(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, shutdown),
    receive
        {'EXIT', Pid, shutdown} -> ok
    after ?TIMEOUT -> ?fail("Should receive 'shutdown'")
    end,
    process_flag(trap_exit, false),
    ok.