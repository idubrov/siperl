%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Functions to log different events happening through the SIP stack
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_log).

%% API
-export([too_big/1, sentby_mismatch/3, no_core/1, bad_request/3, bad_response/3, wrong_vias/1]).

%% Include files
-include("sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%% @doc Report request is too big to be sent through non-reliable
%% transport
%% @end
-spec too_big(#sip_request{}) -> ok.
too_big(#sip_request{}) ->
    error_logger:warning_msg("Message is too big, re-sending using TCP"),
    ok.

%% @doc Report message was discarded, because response Via: header sent-by
%% did not matched value inserted by transport
%% @end
-spec sentby_mismatch(#sip_response{}, tuple(), tuple()) -> ok.
sentby_mismatch(#sip_response{} = Msg, ExpectedSentBy, SentBy) ->
    error_logger:warning_report(['message_discarded',
                                 {reason, sent_by_mismatch},
                                 {expected_sent_by, ExpectedSentBy},
                                 {sent_by, SentBy},
                                 {msg, Msg}]),
    ok.

%% @doc Report message was discarded, because there was no core to handle it.
%% @end
-spec no_core(sip_message()) -> ok.
no_core(Msg) ->
    error_logger:info_report(['message_discarded',
                              {reason, no_core},
                              {msg, Msg}]),
    ok.

%% @doc Report bad request received from remote destination
%% @end
-spec bad_request(sip_message(), any(), #sip_destination{}) -> ok.
bad_request(#sip_request{} = Request, Reason, Remote) ->
    error_logger:warning_report(['bad_request',
                                 {reason, Reason},
                                 {from, Remote},
                                 {msg, Request}]),
    ok.

%% @doc Report bad response received from remote destination
%% @end
-spec bad_response(sip_message(), any(), #sip_destination{}) -> ok.
bad_response(#sip_response{} = Response, Reason, Remote) ->
    error_logger:warning_report(['bad_response',
                                 {reason, Reason},
                                 {from, Remote},
                                 {msg, Response}]),
    ok.

%% @doc Report response was discarded by UAC, because it did not
%% contained exactly one `Via:' header value
%% @end
-spec wrong_vias(#sip_response{}) -> ok.
wrong_vias(Msg) ->
    error_logger:warning_report(['message_discarded',
                                 {reason, wrong_vias},
                                 {msg, Msg}]),
    ok.