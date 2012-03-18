%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc INVITE tests
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_invite_SUITE).

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
    {ok, UA} = sip_test_ua:start_link(fun(Request) -> ?MODULE:Fun(Request) end),
    [{ua, UA} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(ua, Config)),
    ok.


%% @doc Configure UAS to reply with 200 Ok
%% @end
invite_200_handler(Request) ->
    Response = sip_ua:create_response(Request, 200),
    sip_message:append_header('content-type', <<"application/sdp">>, Response).

invite_200(Config) ->
    UA = ?config(ua, Config),

    To = sip_headers:address(<<>>, <<"sip:test_uas@127.0.0.1">>, []),
    {ok, Response} = sip_test_ua:send_invite(UA, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.
