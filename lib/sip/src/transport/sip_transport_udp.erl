%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% UDP transport implementation.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_udp).

%% API
-export([send/2]).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").
-include_lib("kernel/include/inet.hrl").

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec send(#sip_destination{}, #sip_message{}) -> ok | {error, Reason :: term()}.
send(To, Message) when
  is_record(To, sip_destination),
  is_record(Message, sip_message) ->

    % FIXME: Transport layer should receive already resolved IP address!!!
    case To#sip_destination.address of
        {_,_,_,_} ->
            Addr = To#sip_destination.address;
        _ ->
            {ok, #hostent{h_addr_list = [Addr|_]}} = inet:gethostbyname(To#sip_destination.address)
    end,
    To2 = To#sip_destination{address = Addr},
    Pid = lookup_socket(To2),
    sip_transport_udp_socket:send(Pid, To2, Message).


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
            sip_transport_udp_socket_sup:start_socket(To),
            gproc:lookup_pid(Key);
        [P|_] -> P
    end.
