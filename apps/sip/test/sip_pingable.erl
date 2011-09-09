%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pingable UAS implementations. Replies to the OPTIONS
%%% requests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_pingable).

-extends(sip_ua).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([start_link/0, stop/1]).

%% Server callbacks
-export([init/1]).
-export([handle_request/3, handle_cast/2]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    sip_cores:register_core(#sip_core_info{is_applicable = fun is_applicable/1}),
    {ok, #sip_ua_state{callback = ?MODULE, allow = ['OPTIONS']}}.

%% @private
handle_request('OPTIONS', Request, State) ->
    Response = sip_message:create_response(Request, 200),
    pipeline_m:stop({reply, Response, State});
handle_request(_Method, _Request, State) ->
    pipeline_m:next(State).

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast(stop, State) ->
    {stop, normal, State}.


is_applicable(_Msg) -> true.
