%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS default callback
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_uas_callback).

%% API
-export([allow/2, detect_loops/2, supported/2, server/2, is_applicable/1, 'OPTIONS'/2, 'CANCEL'/2, handle_info/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type context() :: term().

-spec is_applicable(context()) -> boolean().
%% @doc Check if this callback is applicable for the request
%% @end
is_applicable(#sip_request{} = _Request) ->
    true.

-spec allow(#sip_request{}, context()) -> [atom()].
%% @doc Return list of methods, supported by this callback
%% @end
allow(_Request, _Context) -> ['OPTIONS', 'CANCEL'].

-spec detect_loops(#sip_request{}, context()) -> boolean().
%% @doc Should UAS detect loops for this request
%% @end
detect_loops(_Request, _Context) ->
    true.

-spec supported(#sip_request{}, context()) -> [atom()].
%% @doc List of supported extensions for this request
%% @end
supported(_Request, _Context) -> [].

-spec server(#sip_request{}, context()) -> binary().
%% @doc Return server name to put into Server: header
%% @end
server(_Request, _Context) ->
    sip_config:server().

-spec 'OPTIONS'(#sip_request{}, context()) -> {noreply, context()} | {reply, #sip_response{}, context()}.
%% @doc Default implementation of `OPTIONS' method.
%% @end
'OPTIONS'(Request, Context) ->
    Response = sip_uas:create_response(Request, 501),
    {reply, Response, Context}.

-spec 'CANCEL'(#sip_request{}, context()) -> {noreply, context()} | {reply, #sip_response{}, context()}.
%% @doc Default implementation of `CANCEL' method.
%% @end
'CANCEL'(Request, Context) ->
    {noreply, Context}.

-spec handle_info(term(), context()) ->
          {noreply, context()} |
          {reply, Request :: #sip_request{}, Response :: #sip_response{}, context()} |
          {stop, Reason :: term(), context()}.
%% @doc Handle arbitrary message
%% @end
handle_info(Info, Context) ->
    {stop, {unexpected, Info}, Context}.
