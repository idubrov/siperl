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
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------
invite(Transport) ->
    request('INVITE', Transport).

%% @doc
%% Generate test request with given `Method'. Sets `Transport' and `Branch' on
%% top via.
%% @end
request(Method, Transport) ->
    % FIXME: Update sip_headers.
    Headers = [{'cseq', sip_headers:cseq(232908, Method)},
               {'via', sip_headers:via(Transport, {<<"127.0.0.1">>, 25060}, [{branch, branch_from_pid()}])},
               {'via', sip_headers:via(udp, {<<"127.0.0.1">>, 5060}, [{branch, <<?MAGIC_COOKIE, $_, "kjshdyff">>}])},
               {'from', sip_headers:address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}])},
               {'to', sip_headers:address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{'tag', <<"839408234">>}])},
               {'call-id', <<"a84b4c76e66710">>},
               {'max-forwards', 70},
               {'content-length', 6}],
    Msg = #sip_message{start_line = sip_message:request(Method, <<"sip:127.0.0.1/test">>),
                       body = <<"Hello!">>,
                       headers = Headers},
    Msg.

%% Extract test process pid from the top branch
pid_from_branch(Msg) when is_record(Msg, sip_message)->
    {ok, Branch} = sip_message:top_via_branch(Msg),
    pid_from_branch(Branch);
pid_from_branch(<<?MAGIC_COOKIE, $_, EncodedPid/binary>>) ->
    Pid = [case C of $A -> $<; $B -> $>; $C -> $.; _ -> C end || <<C>> <= EncodedPid],
    list_to_pid(Pid).

%% @doc
%% Return value of the branch, derived from the process current id. Process id
%% could be restored from the branch value by calling {@link sip_test:pid_from_branch/1}.
%% @end
branch_from_pid() ->
    EncodedPid = << <<case C of $< -> $A; $> -> $B; $. -> $C; _ -> C end>> || C <- pid_to_list(self())>>,
    <<?MAGIC_COOKIE, $_, EncodedPid/binary>>.

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