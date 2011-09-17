%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS default callback
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_uas_callback).

%% API
-export([allowed_methods/2, detect_loops/2, extensions/2, is_applicable/1, 'OPTIONS'/2, handle_info/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type context() :: term().

-spec is_applicable(context()) -> boolean().
%% @doc Check if this callback is applicable for the request
%% @end
is_applicable(#sip_request{} = _Request) ->
    true.

-spec allowed_methods(#sip_request{}, context()) -> [atom()].
%% @doc Return list of methods, supported by this callback
%% @end
allowed_methods(_Request, _Context) -> ['OPTIONS'].

-spec detect_loops(#sip_request{}, context()) -> boolean().
%% @doc Should UAS detect loops for this request
%% @end
detect_loops(_Request, _Context) ->
    true.

-spec extensions(#sip_request{}, context()) -> [atom()].
%% @doc List of supported extensions for this request
%% @end
extensions(_Request, _Context) -> [].

-spec 'OPTIONS'(#sip_request{}, context()) -> {noreply, context()} | {reply, #sip_response{}, context()}.
%% @doc Default implementation of `OPTIONS' method.
%% @end
'OPTIONS'(Request, Context) ->
    Response = sip_message:create_response(Request, 501),
    {reply, Response, Context}.

-spec handle_info(term(), context()) ->
          {noreply, context()} |
          {reply, Request :: #sip_request{}, Response :: #sip_response{}, context()} |
          {stop, Reason :: term(), context()}.
%% @doc Handle arbitrary message
%% @end
handle_info(Info, Context) ->
    {stop, {unexpected, Info}, Context}.
