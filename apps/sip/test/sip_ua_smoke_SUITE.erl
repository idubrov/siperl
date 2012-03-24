%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Basic regression tests for UAC/UAS
%%%
%%% Dialogless requests, simple redirects, going to fallback destinations.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_smoke_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([options_200/1, options_302/1, options_302_failed/1, options_501/1, options_503_next_200/1]).

%% Handlers
-export([options_200_handler/1, options_302_handler/1, options_302_failed_handler/1, options_501_handler/1, options_503_next_200_handler/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [options_200, options_302, options_302_failed, options_501, options_503_next_200].

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

%% @doc
%% Configure UAS to reply with "200 Ok".
%% @end
options_200_handler(Request) ->
    sip_ua:create_response(Request, 200).

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

%% @doc
%% Configure UAS to reply with "301 Moved Temporarily" for username "first" and
%% to reply "200 Ok" on username "second".
%% @end
options_302_handler(#sip_request{uri = #sip_uri{user = <<"first">>}} = Request) ->
    Response = sip_ua:create_response(Request, 302),
    Contact = sip_headers:address(<<>>, <<"sip:second@127.0.0.1">>, []),
    sip_message:append_header(contact, Contact, Response);
options_302_handler(#sip_request{uri = #sip_uri{user = <<"second">>}} = Request) ->
    sip_ua:create_response(Request, 200).

options_302(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.

%% @doc
%% Configure UAS to reply with "301 Moved Temporarily" for username "first" and
%% to reply "200 Ok" on username "second".
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

options_302_failed(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status code
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.

%% @doc
%% Configure UAS to reply with 501 Not Implemented
%% @end
options_501_handler(Request) ->
    sip_ua:create_response(Request, 501).

options_501(Config) ->
    UAC = ?config(uac, Config),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 501, reason = <<"Not Implemented">>} = Response,
    ok.

%% @doc
%% Configure UAS to reply with 503 Service Unavailable for UDP transport
%% @end
options_503_next_200_handler(Request) ->
    Via = sip_message:header_top_value(via, Request),
    case Via#sip_hdr_via.transport of
        udp ->
            sip_ua:create_response(Request, 408);
        tcp ->
            sip_ua:create_response(Request, 200)
    end.

%% RFC 3263 4.3, when transaction layer/transport layer occurs, try next destination
options_503_next_200(Config) ->
    UAC = ?config(uac, Config),

    % Mock sip_resolve to return two destinations for fake URI
    % Both destinations point to 127.0.0.1, but have different transports
    ok = meck:new(sip_resolve, [passthrough]),
    ok = meck:expect(sip_resolve, client_resolve,
                     fun(#sip_uri{host = "example.test"}) ->
                             [#sip_destination{address = {127, 0, 0, 1}, transport = udp},
                              #sip_destination{address = {127, 0, 0, 1}, transport = tcp}]
                     end),

    To = sip_headers:address(<<>>, <<"sip:example.test">>, []),
    {ok, Response} = sip_simple_uac:send_options(UAC, To),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,

    meck:unload(sip_resolve),
    ok.
