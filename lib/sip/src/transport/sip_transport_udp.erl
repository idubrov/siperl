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
    Port = To#sip_destination.port,

    Pid =
        case gproc:lookup_pids({n, l, {udp, Addr, Port}}) of
            [] ->
                % Start new UDP socket for given destination
                % FIXME: could fail in case of race condition!
                {ok, P} = sip_transport_udp_socket_sup:start_socket(To2),
                P;
            [P|_] -> P
        end,
    % Lookup the socket to handle the request
    %{ok, Pid} = gen_server:call(?SERVER, {lookup_socket, To}),
    sip_transport_udp_socket:send(Pid, To2, Message).
