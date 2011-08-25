%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Supervisor for UDP sockets
%%%
%%% For every unique destination address/port we keep a separate
%%% UDP process "connected" to the destination address. That allows
%%% receiving ICMP error messages and propagate them back to the
%%% caller.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_udp_socket_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/0, start_socket/1]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start new socket for given UDP destination.
%%
%% Socket is opened by the socket process and "connected" to the
%% destination, so it will receive ICMP error messages.
%% @end
-spec start_socket(#sip_destination{}) -> {ok, pid()} | {error, any()}.
start_socket(Remote) when is_record(Remote, sip_destination) ->
    supervisor:start_child(?SERVER, [Remote]).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init([]) -> {ok, _}.
init([]) ->
    Module = sip_transport_udp_socket,
    Worker = {Module, {Module, start_link, []}, temporary, 2000, worker, [Module]},
    {ok, {{simple_one_for_one, 1000, 3600}, [Worker]}}.

