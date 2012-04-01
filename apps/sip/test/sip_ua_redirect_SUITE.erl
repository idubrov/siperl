%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Redirections tests
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_redirect_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([options_302/1, options_302_failed/1]).

%% Handlers
-export([options_302_handler/1, options_302_failed_handler/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [options_302, options_302_failed].

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

%% @doc Handler for 302 test
%% UAS replies with `302 Moved Temporarily' when Request-URI has username `first' and
%% with `200 Ok' when username is `second'.
%% @end
options_302_handler(#sip_request{uri = #sip_uri{user = <<"first">>}} = Request) ->
    Response = sip_ua:create_response(Request, 302),
    % next time request should hit the other clause of this handler
    Contact = sip_headers:address(<<>>, <<"sip:second@127.0.0.1">>, []),
    sip_message:append_header(contact, Contact, Response);
options_302_handler(#sip_request{uri = #sip_uri{user = <<"second">>}} = Request) ->
    sip_ua:create_response(Request, 200).

%% @doc Verify that if UAS returns 302, UAC automatically follows the redirect
%% @end
options_302(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.

%% @doc Handler for 302_failed test
%% Redirects to `first' to `second' and `third'. `second' replies with 404,
%% making UAC try other URL (`third')
%% @end
options_302_failed_handler(#sip_request{uri = #sip_uri{user = <<"first">>}} = Request) ->
    Response = sip_ua:create_response(Request, 302),
    Contact1 = sip_headers:address(<<>>, <<"sip:second@127.0.0.1">>, [{q, 0.5}]),
    Contact2 = sip_headers:address(<<>>, <<"sip:third@127.0.0.1">>, [{q, 0.1}]),
    sip_message:append_header(contact, [Contact1, Contact2], Response);
options_302_failed_handler(#sip_request{uri = #sip_uri{user = <<"second">>}} = Request) ->
    sip_ua:create_response(Request, 404);
options_302_failed_handler(#sip_request{uri = #sip_uri{user = <<"third">>}} = Request) ->
    sip_ua:create_response(Request, 200).

%% @doc Verify that if UAS returns 404, UAC tries other URIs provided in the original redirect
%% response.
%% @end
options_302_failed(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status code
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.
