%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% UDP transport implementation.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_udp).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Transport callbacks
-export([send/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

%% Records
-record(state, {ports, supervisor}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec start_link([integer()], pid()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ports, Sup) when is_list(Ports), is_pid(Sup) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Ports, Sup}, []).

%%-----------------------------------------------------------------
%% Transport callbacks
%%-----------------------------------------------------------------
-spec send(#sip_destination{}, #sip_message{}) -> {ok, #sip_destination{}} | {error, Reason :: term()}.
send(To, Message) when
  is_record(To, sip_destination),
  is_record(Message, sip_message) ->
    % Lookup the socket to handle the request
    {ok, Pid} = gen_server:call(?SERVER, {lookup_socket, To}),
    sip_transport_udp_socket:send(Pid, To, Message).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({[integer()], pid()}) -> {ok, #state{}}.
init({Ports, Sup}) ->
    {ok, #state{ports = Ports, supervisor = Sup}}.

%% @private
-spec handle_call({lookup_socket | term(), #sip_destination{}}, _, #state{}) ->
          {reply, #sip_destination{} | false, #state{}}.
handle_call({lookup_socket, _To}, _From, State) ->
    % Consult parent supervisor for children named like {socket, _}
    Endpoint = lookup_socket(supervisor:which_children(State#state.supervisor)),
    {reply, Endpoint, State};

%% @private
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
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

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% Lookup for the first UDP transport available.
lookup_socket([]) ->
    false;

lookup_socket([{{socket, _}, Child, _Type, _Modules} | _]) ->
    {ok, Child};

lookup_socket([_|T]) ->
    lookup_socket(T).