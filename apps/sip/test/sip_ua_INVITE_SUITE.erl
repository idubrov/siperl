%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc INVITE tests
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_INVITE_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([invite_200/1]).
-export([invite_200_handler/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [invite_200].

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
    {ok, UAS} = sip_simple_uas:start_link(fun(Request, ReplyFun) -> ReplyFun(?MODULE:Fun(Request)) end),
    [{uac, UAC}, {uas, UAS} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(uac, Config)),
    ok = sip_test:shutdown(?config(uas, Config)),
    ok.


%% @doc Configure UAS to reply with 200 Ok
%% @end
invite_200_handler(Request) ->
    Response = sip_ua:create_response(Request, 200),
    sip_message:append_header('content-type', <<"application/sdp">>, Response).

invite_200(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:test_uas@127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_invite(UAC, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,

    % FIXME: verify that ACK is actually sent to Contact: value...
    % send ACK
    ACK = sip_ua:create_ack(Response),
    {ok, _Ref} = sip_ua:send_request(ACK),
    timer:sleep(500),
    ok.

% FIXME: 12.2.2, invalid To tag in request?