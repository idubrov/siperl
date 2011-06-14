%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Registry that tracks active TCP connections and indexes them
%%% by the {address, port, transport} tuple.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_tcp_conn_registry).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

% API
-export([start_link/0, register/3, lookup/1, list/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip_transport.hrl").

%%-----------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------

%% connections is mapping from RemoteEndpoint :: #sip_endpoint{} -> {pid(), LocalEndpoint :: #sip_endpoint{}}
%% pids is mapping from pid() -> RemoteEndpoint :: #sip_endpoint{} 
-record(state, {connections = dict:new(), pids = dict:new()}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register(#sip_endpoint{}, #sip_endpoint{}, pid()) -> ok.
register(LocalEndpoint, RemoteEndpoint, Pid) 
  when is_record(LocalEndpoint, sip_endpoint), 
	   is_record(RemoteEndpoint, sip_endpoint), 
	   is_pid(Pid) ->
	gen_server:call(?SERVER, {monitor, LocalEndpoint, RemoteEndpoint, Pid}).

-spec lookup(RemoteEndpoint :: #sip_endpoint{}) -> 
		  {ok, [{Pid :: pid(), LocalEndpoint :: #sip_endpoint{}}]} | error.
lookup(RemoteEndpoint) when is_record(RemoteEndpoint, sip_endpoint) ->
	gen_server:call(?SERVER, {lookup, RemoteEndpoint}).

-spec list() ->
	[RemoteEndpoint :: #sip_endpoint{}].
list() ->
	gen_server:call(?SERVER, list).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(_) -> {ok, #state{}}.
init([]) ->
	{ok, #state{}}.

%% @private
-spec handle_call(_, _, #state{}) -> {reply, _, #state{}} | {stop, _, #state{}}.
handle_call({monitor, LocalEndpoint, RemoteEndpoint, Pid}, _From, State) ->
	erlang:monitor(process, Pid),
	% Could be several connections for same remote endpoint
	Conns = dict:append(RemoteEndpoint, {Pid, LocalEndpoint}, State#state.connections),
	% One pid - one connection
	Pids = dict:store(Pid, RemoteEndpoint, State#state.pids),
    {reply, ok, State#state{connections = Conns, pids = Pids}};

handle_call({lookup, RemoteEndpoint}, _From, State) ->
	Result = dict:find(RemoteEndpoint, State#state.connections),
	{reply, Result, State};

handle_call(list, _From, State) ->
	Result = dict:fetch_keys(State#state.connections),
    {reply, Result, State};

handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> 
		  {noreply, #state{}} | {stop, {unexpected, _}, #state{}}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
	% clear pid -> remote index mapping
	RemoteEndpoint = dict:fetch(Pid, State#state.pids),
	Pids = dict:erase(Pid, State#state.pids),
	
	% remove from the remote index -> data mapping
	List = dict:fetch(RemoteEndpoint, State#state.connections),
	Conns = case lists:keydelete(Pid, 1, List) of
				[] -> 
					dict:erase(RemoteEndpoint, State#state.connections);
				
				List2 -> 
					dict:store(RemoteEndpoint, List2, State#state.connections)
	end,		
	{noreply, State#state{connections = Conns, pids = Pids}};

handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
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
