%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS default callback
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_default).

%% API
-export([allow/1, supported/1, server/1, is_applicable/1]).
-export(['INVITE'/2, 'OPTIONS'/2, 'CANCEL'/2]).
-export([handle_response/4, handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-type state() :: term().
-type gen_from() :: {pid(), term()}.

-spec is_applicable(#sip_request{}) -> boolean().
%% @doc Check if this callback is applicable for the request
%% @end
is_applicable(#sip_request{} = _Request) -> true.

-spec allow(#sip_request{}) -> [atom()].
%% @doc Return list of methods, supported by this callback
%% @end
allow(_Request) -> ['INVITE', 'BYE', 'OPTIONS', 'CANCEL'].

-spec supported(#sip_request{}) -> [atom()].
%% @doc List of supported extensions for this request
%% @end
supported(_Request) -> [].

-spec server(#sip_request{}) -> binary().
%% @doc Return server name to put into Server: header
%% @end
server(_Request) ->
    sip_config:server().

-spec 'INVITE'(#sip_request{}, state()) -> {noreply, state()} | {reply, #sip_response{}, state()}.
%% @doc Default implementation of `INVITE' method.
%% @end
'INVITE'(Request, State) ->
    Response = sip_ua:create_response(Request, 501),
    {reply, Response, State}.

-spec 'OPTIONS'(#sip_request{}, state()) -> {noreply, state()} | {reply, #sip_response{}, state()}.
%% @doc Default implementation of `OPTIONS' method.
%% @end
'OPTIONS'(Request, State) ->
    Response = sip_ua:create_response(Request, 200),
    {reply, Response, State}.

-spec 'CANCEL'(#sip_request{}, state()) -> {noreply, state()} | {reply, #sip_response{}, state()}.
%% @doc Default handling of `CANCEL' method.
%% @end
'CANCEL'(Request, State) ->
    Status =
        case sip_transaction:cancel(Request) of
            false -> 481;       % Call/Transaction Does Not Exist
            {ok, _TxKey} -> 200 % Ok
        end,
    Response = sip_ua:create_response(Request, Status),
    {reply, Response, State}.

-spec handle_response(sip_name(), #sip_response{}, reference(), state()) -> {noreply, state()}.
%% @doc Handle response received by UAC
%% @end
handle_response(_Method, _Response, _RequestId, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) ->
          {noreply, state()} |
          {stop, Reason :: term(), state()}.
%% @doc Handle arbitrary message
%% @end
handle_info(Info, State) ->
    {stop, {unexpected, Info}, State}.

-spec handle_call(term(), gen_from(), state()) ->
          {noreply, state()} |
          {reply, Reply :: term(), state()} |
          {stop, Reason :: term(), state()}.
%% @doc Handle calls
%% @end
handle_call(Call, _From, State) ->
    {stop, {unexpected, Call}, State}.

%% @doc Handle casts
%% @end
-spec handle_cast(term(), state()) ->
          {noreply, state()} |
          {stop, Reason :: term(), state()}.
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.
