%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc INVITE tests
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_invite_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([invite_200/1]).

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

init_per_testcase(_TestCase, Config) ->
    {ok, UAC} = sip_uac:start_link(),
    [{uac, UAC} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(uac, Config)),
    ok.

invite_200(Config) ->
    UACPid = ?config(uac, Config),
    UAC = sip_headers:address(<<>>, <<"sip:uac@127.0.0.1">>, []),
    UAS = sip_headers:address(<<>>, <<"sip:uas@127.0.0.1">>, []),

    % Start UAS to reply with 200 Ok
    Handler =
        fun (Request) ->
                 Response = sip_uas:create_response(Request, 200),
                 sip_message:append_header(contact, UAS, Response)
        end,
    sip_uas:start_link(sip_test_uas, Handler),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    Request = sip_uac:create_request(UACPid, 'INVITE', To),
    Request2 = sip_message:append_header(contact, UAC, Request),

    % Responses will be delivered via messages
    {ok, Ref} = sip_uac:send_request(UACPid, Request2),
    ok = receive
             {response, Ref, #sip_response{status = 200}} ->
                 ok
         after 5000 -> {error, response_expected}
         end,
    ok.
