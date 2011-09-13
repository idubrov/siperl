%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pingable UAS implementations. Replies to the OPTIONS
%%% requests.
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(sip_test_uas).

-extends(sip_ua).

%% Exports

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%% API
-export([start_link/0, set_handler/2, stop/1]).

%% Server callbacks
-export([init/1]).
-export([handle_request/3, handle_cast/2, handle_call/3]).

-record(state, {handler}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

set_handler(Pid, Fun) ->
    gen_server:call(Pid, {set_handler, Fun}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    sip_cores:register_core(#sip_core_info{is_applicable = fun is_applicable/1}),
    {ok, #sip_ua_state{callback = ?MODULE,
                       allow = ['OPTIONS'],
                       state = #state{},
                       detect_loops = false}}.

%% @private
handle_call({set_handler, Handler}, _Client, #sip_ua_state{} = State) ->
    UserState = State#sip_ua_state.state,
    {reply, ok, State#sip_ua_state{state = UserState#state{handler = Handler}}}.

%% @private
handle_request(_Method, Request, #sip_ua_state{state = #state{handler = Handler}} = State) ->
    Response = Handler(Request),
    pipeline_m:stop({reply, Response, State}).

%% @private
-spec handle_cast(_, #sip_ua_state{}) -> any().
handle_cast(stop, State) ->
    {stop, normal, State}.


is_applicable(_Msg) -> true.
