%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Basic regression tests for UAC/UAS
%%%
%%% Dialogless requests, simple redirects, going to fallback destinations.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_basic_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([options_200/1, options_302/1, options_302_failed/1, options_501/1, options_503_next_200/1]).

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

init_per_testcase(_TestCase, Config) ->
    {ok, UAC} = sip_uac:start_link(),
    [{uac, UAC} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = sip_test:shutdown(?config(uac, Config)),
    ok.

options_200(Config) ->
    UAC = ?config(uac, Config),

    % Start UAS to reply with 200 Ok
    Handler =
        fun (Request) ->
                 sip_message:create_response(Request, 200)
        end,
    sip_uas:start_link(sip_test_uas, Handler),


    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    Request = sip_uac:create_request(UAC, 'OPTIONS', To),
    {ok, Response} = sip_uac:send_request_sync(UAC, Request),

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
    ['INVITE', 'OPTIONS'] = sip_message:header_values(allow, Response),

    _Bin = sip_message:header_top_value('call-id', Response),
    ok.

options_302(Config) ->
    UAC = ?config(uac, Config),

    % configure UAS to reply with "301 Moved Temporarily" for username "first" and
    % to reply "200 Ok" on username "second".
    Handler =
        fun(#sip_request{uri = #sip_uri{user = <<"first">>}} = Request) ->
                Response = sip_message:create_response(Request, 302),
                Contact = sip_headers:address(<<>>, <<"sip:second@127.0.0.1">>, []),
                sip_message:append_header(contact, Contact, Response);
           (#sip_request{uri = #sip_uri{user = <<"second">>}} = Request) ->
                sip_message:create_response(Request, 200)
        end,
    sip_uas:start_link(sip_test_uas, Handler),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    Request = sip_uac:create_request(UAC, 'OPTIONS', To),
    {ok, Response} = sip_uac:send_request_sync(UAC, Request),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.

options_302_failed(Config) ->
    UAC = ?config(uac, Config),

    % configure UAS to reply with "301 Moved Temporarily" for username "first" and
    % to reply "200 Ok" on username "second".
    Handler =
        fun(#sip_request{uri = #sip_uri{user = <<"first">>}} = Request) ->
                Response = sip_message:create_response(Request, 302),
                Contact1 = sip_headers:address(<<>>, <<"sip:second@127.0.0.1">>, [{q, 0.5}]),
                Contact2 = sip_headers:address(<<>>, <<"sip:third@127.0.0.1">>, [{q, 0.1}]),
                sip_message:append_header(contact, [Contact1, Contact2], Response);
           (#sip_request{uri = #sip_uri{user = <<"second">>}} = Request) ->
                sip_message:create_response(Request, 404);
           (#sip_request{uri = #sip_uri{user = <<"third">>}} = Request) ->
                sip_message:create_response(Request, 200)
        end,
    sip_uas:start_link(sip_test_uas, Handler),

    To = sip_headers:address(<<>>, <<"sip:first@127.0.0.1">>, []),
    Request = sip_uac:create_request(UAC, 'OPTIONS', To),
    {ok, Response} = sip_uac:send_request_sync(UAC, Request),

    % validate status code
    #sip_response{status = 200, reason = <<"Ok">>} = Response,
    ok.

options_501(Config) ->
    UAC = ?config(uac, Config),

    % configure UAS to reply with 501 Not Implemented
    Handler = fun (Request) -> sip_message:create_response(Request, 501) end,
    sip_uas:start_link(sip_test_uas, Handler),

    To = sip_headers:address(<<>>, <<"sip:127.0.0.1">>, []),
    Request = sip_uac:create_request(UAC, 'OPTIONS', To),
    {ok, Response} = sip_uac:send_request_sync(UAC, Request),

    % validate status
    #sip_response{status = 501, reason = <<"Not Implemented">>} = Response,
    ok.

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

    % configure UAS to reply with 503 Service Unavailable for UDP transport
    Handler =
        fun (Request) ->
                 Via = sip_message:header_top_value(via, Request),
                 case Via#sip_hdr_via.transport of
                     udp ->
                         sip_message:create_response(Request, 408);
                     tcp ->
                         sip_message:create_response(Request, 200)
                 end
        end,
    sip_uas:start_link(sip_test_uas, Handler),

    To = sip_headers:address(<<>>, <<"sip:example.test">>, []),
    Request = sip_uac:create_request(UAC, 'OPTIONS', To),
    {ok, Response} = sip_uac:send_request_sync(UAC, Request),

    % validate status
    #sip_response{status = 200, reason = <<"Ok">>} = Response,

    meck:unload(sip_resolve),
    ok.
