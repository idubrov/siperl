%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% UDP transport implementation.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport_udp).

%% API
-export([send/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").
-include_lib("kernel/include/inet.hrl").

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec send(#sip_destination{}, sip_message()) -> ok | {error, Reason :: term()}.
send(To, Message) when
  is_record(To, sip_destination),
  (is_record(Message, sip_request) orelse is_record(Message, sip_response)) ->

    [Port | _] = sip_config:ports(udp),
    Pid = gproc:lookup_pid({n, l, {udp, Port}}),
    sip_transport_udp_socket:send(Pid, To, Message).
