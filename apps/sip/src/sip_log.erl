%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Functions to log events happening through the SIP stack implementation
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_log).

%% API
-export([bad_request/3, bad_response/3]).

%% Include files
-include("sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------


%% @doc Log bad request received from remote destination
%% @end
-spec bad_request(sip_message(), any(), #sip_destination{}) -> ok.
bad_request(#sip_request{} = Request, Reason, Remote) ->
    error_logger:warning_report(['bad_request',
                                 {reason, Reason},
                                 {from, Remote},
                                 {msg, Request}]),
    ok.

%% @doc Log bad response received from remote destination
%% @end
-spec bad_response(sip_message(), any(), #sip_destination{}) -> ok.
bad_response(#sip_response{} = Response, Reason, Remote) ->
    error_logger:warning_report(['bad_response',
                                 {reason, Reason},
                                 {from, Remote},
                                 {msg, Response}]),
    ok.
