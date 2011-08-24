%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc Pingable UAS implementations. Replies to the OPTIONS
%%% requests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_pingable).

-extends(sip_ua).

%% Exports

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%% API
-export([start_link/0]).

%% Server callbacks
-export([init/1]).
-export([handle_request/3]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    sip_cores:register_core(#sip_core_info{is_applicable = fun is_applicable/1}),
    #sip_ua_state{callback = ?MODULE}.

%% @private
handle_request('OPTIONS', Request, State) ->
    Response = sip_message:create_response(Request, 200, <<"Ok. Hello!">>),
    sip_ua:send_response(Response),
    {noreply, State};
handle_request(Method, Request, State) ->
    sip_ua:handle_request(Method, Request, State).

is_applicable(#sip_message{kind = #sip_request{method = 'OPTIONS'}}) -> true;
is_applicable(_Msg) -> false.
