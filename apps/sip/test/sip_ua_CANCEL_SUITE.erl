%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc CANCEL method tests. Note that implementation of `CANCEL' processing is in sip_simple_uas.
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua_CANCEL_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([apply_fun/2]).
-export([cancel_487/1, cancel_487_handler/2, cancel_481/1, cancel_200/1, cancel_200_handler/2]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [cancel_487, cancel_481, cancel_200].

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
    {ok, UAC} = sip_simple_uac:start_link(),
    {ok, UAS} = sip_simple_uas:start_link(fun(Request, ReplyFun) -> ?MODULE:Fun(Request, ReplyFun) end),
    [{uac, UAC}, {uas, UAS} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(uac, Config)),
    ok = sip_test:shutdown(?config(uas, Config)),
    ok.

apply_fun(Fun, Args) ->
    erlang:apply(Fun, Args).

%% @doc The 200 response should be overriden by 487 Request Terminated
%% @end
cancel_487_handler(Request, ReplyFun) ->
    Response = sip_ua:create_response(Request, 200),
    Response2 = sip_message:append_header('content-type', <<"application/sdp">>, Response),
    timer:apply_after(2000, erlang, apply, [ReplyFun, [Response2]]),
    ok.

%% @doc Verify that CANCEL on active INVITE requests forces INVITE to return status 487
%% @end
cancel_487(Config) ->
    UAC = ?config(uac, Config),

    % cancel the request after 500 milliseconds
    {ok, _TRef} = timer:apply_after(500, sip_simple_uac, cancel, [UAC]),
    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_invite(UAC, To),

    % validate INVITE response status
    #sip_response{status = 487, reason = <<"Request Terminated">>} = Response,
    timer:sleep(5000),
    ok.

%% @doc Do not reply on INVITE request (it will be cancelled)
%% @end
cancel_200_handler(#sip_request{method = 'INVITE'} = Request, ReplyFun) ->
    Response = sip_ua:create_response(Request, 200),
    Response2 = sip_message:append_header('content-type', <<"application/sdp">>, Response),
    ReplyFun(Response2),
    ok.

%% @doc Verify that CANCEL on responded INVITE has no effect
%% @end
cancel_200(Config) ->
    UAC = ?config(uac, Config),

    % send INVITE and receive response
    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_invite(UAC, To),
    #sip_response{status = 200, reason = <<"Ok">>} = Response,

    % cancel the request
    ok = sip_simple_uac:cancel(UAC),
    timer:sleep(500), % wait until CANCEL is processed
    ok.

%% @doc Verify that CANCEL on non-existent transaction returns status 481
%% @end
cancel_481(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    Invite = sip_ua:create_request('INVITE', To),
    Cancel = sip_message:create_cancel(Invite),
    {ok, Response} = sip_simple_uac:send_request(UAC, Cancel),
    #sip_response{status = 481, reason = <<"Call/Transaction Does Not Exist">>} = Response,
    ok.
