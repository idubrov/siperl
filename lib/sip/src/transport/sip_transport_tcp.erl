%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% TCP transport implementation module.
%%% ports.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transport_tcp).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([send/2]).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec send(#sip_destination{} | pid(), #sip_message{}) -> ok | {error, Reason :: term()}.
send(To, Message) when is_record(To, sip_destination) ->
    {ok, Pid} = sip_transport_tcp_conn_sup:start_connection(To),
    send(Pid, Message);
send(Pid, Message) when is_pid(Pid), is_record(Message, sip_message) ->
    sip_transport_tcp_conn:send(Pid, Message).
