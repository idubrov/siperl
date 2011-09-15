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
    Pid = lookup_socket(To),
    sip_transport_udp_socket:send(Pid, To, Message).


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
lookup_socket(To) ->
    Key = {n, l, {udp, To#sip_destination.address, To#sip_destination.port}},

    % Lookup for the socket
    case gproc:lookup_pids(Key) of
        [] ->
            % Start new UDP socket for given destination
            % Note that this could actually fail in case of race condition
            % So we query gproc again to get the pid of surviving socket
            _Ignore = sip_transport_udp_socket_sup:start_socket(To),
            gproc:lookup_pid(Key);
        [P|_] -> P
    end.
