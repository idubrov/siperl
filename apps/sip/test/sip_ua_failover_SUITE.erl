%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Basic failover tests for UAC/UAS
%%%
%%%
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_ua_failover_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([options_503_next_200/1]).

%% Handlers
-export([options_503_next_200_handler/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

-define(HOST, "sip.example.org").

%% Common tests
all() ->
    [options_503_next_200].

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

%% @doc UAS handler for failover test
%% Reply with 503 Service Unavailable when transport is UDP
%% @end
options_503_next_200_handler(Request) ->
    Via = sip_message:header_top_value(via, Request),
    case Via#sip_hdr_via.transport of
        udp ->
            sip_ua:create_response(Request, 408);
        tcp ->
            sip_ua:create_response(Request, 200)
    end.

%% @doc Transport failover test
%% Send request to the destination that returns failure for UDP
%% @end
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
