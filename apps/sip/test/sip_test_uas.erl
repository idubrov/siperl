%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS callback implementation used for testing
%%%
%%% Delegates response generation to configured function.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_uas).
-extends(sip_uas_callback).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([init/1, detect_loops/2, 'OPTIONS'/2]).

-record(context, {handler}).

init(Handler) ->
    {ok, #context{handler = Handler}}.

detect_loops(_Request, _State) ->
    false.

'OPTIONS'(Request, #context{handler = Handler} = Context) ->
    Response = Handler(Request),
    {reply, Response, Context}.
