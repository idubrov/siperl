%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP transport layer supervisor.
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
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

-include("../sip_sup_specs.hrl").

%% API functions
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link(?MODULE, {}).

%% Supervisor callbacks

%% @private
-spec init({}) -> {ok, _}.
init({}) ->
    % Start endpoint supervisors
    UDP = sip_config:ports(udp),
    TCP = sip_config:ports(tcp),

    Children = [?SUPERVISOR(sip_transport_udp_sup, [UDP]),
                ?SUPERVISOR(sip_transport_tcp_sup, [TCP]),
                ?SERVER(sip_transport, sip_transport, []),
                ?SERVER(sip_transport_icmp, sip_transport_icmp, [])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
