%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Smoke tests for UAC/UAS implementations.
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_smoke_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([options_200/1, options_501/1]).

%% Handlers
-export([options_200_handler/1, options_501_handler/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [options_200, options_501].

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

%% @doc UAS handler for 200 test
%% Reply with `200 Ok'.
%% @end
options_200_handler(Request) ->
    sip_ua:create_response(Request, 200).

%% @doc Verify basic request/response
%% @end
options_200(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,

    % validate headers
    0 = sip_message:header_top_value('content-length', Response),

    [Via] = sip_message:header_values(via, Response),
    #sip_hdr_via{transport = udp, host = ?HOST, port = 5060} = Via,
    {received, {127, 0, 0, 1}} = lists:keyfind(received, 1, Via#sip_hdr_via.params),

    #sip_hdr_address{} = sip_message:header_top_value(to, Response),
    #sip_hdr_address{} = sip_message:header_top_value(from, Response),
    #sip_hdr_cseq{method = 'OPTIONS'} = sip_message:header_top_value(cseq, Response),
    ['INVITE', 'BYE', 'OPTIONS', 'CANCEL'] = sip_message:header_values(allow, Response),

    _Bin = sip_message:header_top_value('call-id', Response),
    ok.

%% @doc Handler for 501 test
%% Reply with `501 Not Implemented'.
%% @end
options_501_handler(Request) ->
    sip_ua:create_response(Request, 501).

%% @doc Verify that error responses are properly processed
%% @end
options_501(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 501, reason = <<"Not Implemented">>} = Response,
    ok.
