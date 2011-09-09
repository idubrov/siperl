%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Simple (dialogless) regression tests for UA
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_simple_ua_SUITE).

%% Exports
-export([all/0, init_per_suite/1, end_per_suite/1, request_response/1]).

%% Include files
-include_lib("common_test/include/ct.hrl").
-include("sip.hrl").

%% Common tests
all() ->
    [request_response].

init_per_suite(Config) ->
    ok = application:start(gproc),
    ok = application:start(sip),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(sip),
    ok = application:stop(gproc),
    ok.

request_response(_Config) ->
    {ok, UAS} = sip_pingable:start_link(),
    {ok, UAC} = sip_pinger:start_link(),

    Address = sip_headers:address(<<"">>, <<"sip:127.0.0.1">>, []),
    {ok, Response} = sip_pinger:ping(UAC, Address),

    #sip_response{status = 200, reason = <<"Ok">>} = Response#sip_message.kind,

    0 = sip_message:header_top_value('content-length', Response),

    [Via] = sip_message:header_values(via, Response),
    #sip_hdr_via{transport = udp, host = "ISDNOTE", port = 5060} = Via,
    {received, {127, 0, 0, 1}} = lists:keyfind(received, 1, Via#sip_hdr_via.params),

    To = sip_message:header_top_value(to, Response),
    #sip_hdr_address{} = To,

    From = sip_message:header_top_value(from, Response),
    #sip_hdr_address{} = From,

    CSeq = sip_message:header_top_value(cseq, Response),
    #sip_hdr_cseq{method = 'OPTIONS'} = CSeq,

    Allow = sip_message:header_values(allow, Response),
    ['OPTIONS'] = Allow,

    _Bin = sip_message:header_top_value('call-id', Response),

    ok = sip_pinger:stop(UAC),
    ok = sip_pingable:stop(UAS),
    ok.

