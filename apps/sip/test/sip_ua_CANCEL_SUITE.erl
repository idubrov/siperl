%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc CANCEL method tests
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua_CANCEL_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([apply_fun/2]).
-export([cancel_487/1, cancel_487_handler/2, cancel_481/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [cancel_487, cancel_481].

init_per_suite(Config) ->
    ok = application:start(gproc),
    ok = application:start(sip),
    ok = application:set_env(sip, self, ?HOST),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(sip),
    ok = application:stop(gproc),
    ok.

init_per_testcase(TestCase, Config) ->
    Fun = list_to_atom(atom_to_list(TestCase) ++ "_handler"),
    {ok, UA} = sip_test_ua:start_link(fun(Request, ReplyFun) -> ?MODULE:Fun(Request, ReplyFun) end),
    [{ua, UA} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(ua, Config)),
    ok.

apply_fun(Fun, Args) ->
    erlang:apply(Fun, Args).

%% @doc Do not reply on INVITE request (it will be cancelled)
%% @end
cancel_487_handler(#sip_request{method = 'INVITE'}, _ReplyFun) ->
    ok.

%% @doc Verify that CANCEL on active INVITE requests forces INVITE to return status 487
%% @end
cancel_487(Config) ->
    UA = ?config(ua, Config),

    % cancel the request after 500 milliseconds
    {ok, _TRef} = timer:apply_after(500, sip_test_ua, cancel, [UA]),
    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_test_ua:send_invite(UA, To),

    % validate INVITE response status
    #sip_response{status = 487, reason = <<"Request Terminated">>} = Response,
    timer:sleep(5000),
    ok.

%% @doc Verify that CANCEL on non-existent transaction returns status 481
%% @end
cancel_481(Config) ->
    UA = ?config(ua, Config),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    Invite = sip_ua:create_request('INVITE', To),
    Cancel = sip_message:create_cancel(Invite),
    {ok, Response} = sip_test_ua:send_request(UA, Cancel),
    #sip_response{status = 481, reason = <<"Call/Transaction Does Not Exist">>} = Response,
    ok.

% FIXME: CANCEL after 2xx!