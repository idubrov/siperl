%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
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
-export([start_link/1]).

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
-include_lib("sip_transport.hrl").
-include_lib("sip_message.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec start_link([{Transport :: atom(), [Opts :: term()]}]) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Opts) when is_list(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%-----------------------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------------------

%% @private
-spec init(sip_config:config()) -> {ok, _}.
init(Cfg) ->
	% Start endpoint supervisors	
	UDP = sip_config:ports(Cfg, udp),
	TCP = sip_config:ports(Cfg, tcp),
	
	Children = [?SPEC(sip_transport_udp_sup, supervisor, [UDP]),
				?SPEC(sip_transport_tcp_sup, supervisor, [TCP]),
				?WORKER(sip_transport, [Cfg])],
    {ok, {{one_for_one, 1000, 3600}, Children}}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
