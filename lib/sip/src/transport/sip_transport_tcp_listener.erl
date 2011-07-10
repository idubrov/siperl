%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_tcp_listener).

-behaviour(gen_server).

%% Server callbacks
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include_lib("sip.hrl").

%% Types
-record(state, {socket :: inet:socket()}).

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

%% @private
-spec init(integer()) -> {ok, #state{}}.
init(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, inet]),

    % Start infinite accept cycle
    gen_server:cast(self(), accept),
    {ok, #state{socket = Socket}}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_call(_, _, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_cast(accept, State) ->
    Socket = State#state.socket,
    case gen_tcp:accept(Socket, 1000) of
        {ok, Sock} ->
            sip_transport_tcp_conn_sup:start_connection(Sock);

        {error, timeout} ->
            ok
    end,
    gen_server:cast(self(), accept),
    {noreply, State};

%% @private
handle_cast(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
