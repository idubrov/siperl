%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS default callback
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_uas_callback).

%% API
-export([allowed_methods/2, detect_loops/2, extensions/2, is_applicable/1, 'OPTIONS'/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type context() :: term().

%% @doc Check if this callback is applicable for the request
%% @end
-spec is_applicable(context()) -> boolean().
is_applicable(#sip_request{} = _Request) ->
    true.

%% @doc Return list of methods, supported by this callback
%% @end
-spec allowed_methods(#sip_request{}, context()) -> [atom()].
allowed_methods(_Request, _Context) -> ['OPTIONS'].

%% @doc Should UAS detect loops for this request
%% @end
-spec detect_loops(#sip_request{}, context()) -> boolean().
detect_loops(_Request, _Context) ->
    true.

%% @doc List of supported extensions for this request
%% @end
-spec extensions(#sip_request{}, context()) -> [atom()].
extensions(_Request, _Context) -> [].

%% @doc Default implementation of `OPTIONS' method.
%% @end
-spec 'OPTIONS'(#sip_request{}, context()) -> {noreply, context()} | {reply, #sip_response{}, context()}.
'OPTIONS'(Request, Context) ->
    Response = sip_message:create_response(Request, 501),
    {reply, Response, Context}.
