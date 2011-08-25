%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP transport layer supervisor.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_sup).

-behaviour(supervisor).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("../sip_common.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, _}.
init({}) ->
    % Start endpoint supervisors
    UDP = sip_config:ports(udp),
    TCP = sip_config:ports(tcp),

    Children = [?SPEC(sip_transport_udp_sup, supervisor, [UDP]),
                ?SPEC(sip_transport_tcp_sup, supervisor, [TCP]),
                ?WORKER(sip_transport, [])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
