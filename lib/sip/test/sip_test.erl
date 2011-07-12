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
%% Extract test process pid from the top branch
test_pid(Msg) ->
    <<?MAGIC_COOKIE, $_, Bin/binary>> = sip_headers:top_via_branch(Msg#sip_message.headers),
    Pid = [case C of $A -> $<; $B -> $>; $C -> $.; _ -> C end || <<C>> <= Bin],
    list_to_pid(Pid).

invite(Transport) ->
    request('INVITE', Transport).

%% @doc
%% Also encodes current process id into the branch id, so it could be restored later
%% to route message back to the test process
%% end
request(Method, Transport) ->
    ViaTop = with_test_branch(sip_headers:via(Transport, {<<"127.0.0.1">>, 25060}, [])),
    Via = sip_headers:via(udp, {<<"127.0.0.1">>, 5060}, [{branch, <<?MAGIC_COOKIE, $_, "kjshdyff">>}]),
    CSeq = sip_headers:cseq(232908, Method),
    From = sip_headers:from(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
    To = sip_headers:to(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{'tag', <<"839408234">>}]),
    ContentLength = sip_headers:content_length(6),

    % FIXME: Update sip_headers.
    CallId = {'call-id', <<"a84b4c76e66710">>},
    MaxForwards = {'max-forwards', 70},

    Headers = [CSeq, ViaTop, Via, From, To, CallId, MaxForwards, ContentLength],
    Msg = #sip_message{start_line = sip_message:request(Method, <<"sip:127.0.0.1/test">>),
                       body = <<"Hello!">>,
                       headers = Headers},
    Msg.

%% @doc
%% Append branch to the top Via: header. Branch is generated from the process id (which could be restored
%% from branch by calling {@link sip_test:test_pid/1}.
%% @end
with_test_branch({'via', [Via]}) ->
    Unique = << <<case C of $< -> $A; $> -> $B; $. -> $C; _ -> C end>> || C <- pid_to_list(self())>>,
    Params = [{branch, <<?MAGIC_COOKIE, $_, Unique/binary>>} | Via#sip_hdr_via.params],
    {'via', [Via#sip_hdr_via{params = Params}]}.

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