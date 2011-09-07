%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% TCP transport implementation module.
%%% ports.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transport_tcp).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([send/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec send(#sip_destination{} | #sip_connection{}, #sip_message{}) -> ok | {error, Reason :: term()}.
send(To, Message) when is_record(To, sip_destination) ->
    case sip_transport_tcp_conn_sup:start_connection(To) of
        {ok, Pid} ->
            sip_transport_tcp_conn:send(Pid, Message);
        {error, Reason} ->
            {error, Reason}
    end;
send(Conn, Message) when is_record(Conn, sip_connection),
                         is_record(Message, sip_message) ->
    sip_transport_tcp_conn:send(Conn#sip_connection.connection, Message).
