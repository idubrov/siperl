%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP transport layer router behaviour. Transport layer router is
%%% responsible for routing messages between transport layer and its
%%% users (typically it is transaction layer, but could be stateless
%%% proxy, for example).
%%% This module also be used as a default implementation of routing.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_router).

-behaviour(sip_router).

%% Router callbacks
-export([handle/3]).
-export([send_request/3, send_response/2]).
-export([behaviour_info/1]).

%% Includes
-include_lib("sip_common.hrl").
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").

%%-----------------------------------------------------------------
%% Behaviour callbacks
%%-----------------------------------------------------------------

-spec behaviour_info(term()) -> list() | undefined.
behaviour_info(callbacks) ->
    [{handle, 3}, 							% Transport layer callback
	 {send_request, 3}, {send_response, 2}	% Transport layer API
	];

behaviour_info(_) ->
    undefined.

%% Router implementation

%% @doc
%% Handle the request/response received by the transport layer.
%% @end
-spec handle(sip_transport:connection(), #sip_endpoint{}, #sip_message{}) -> ok.
handle(Conn, Remote, Msg) ->
	case sip_transaction:handle(Conn, Remote, Msg) of
		false ->
			% pass to core, transaction layer have not processed the message
			sip_core:handle(Conn, Remote, Msg);
		_ -> 
			% handled by tx layer
			ok
	end.

%% @doc
%% Send the request to the transport layer.
%% @end
-spec send_request(sip_transport:connection() | undefined, #sip_endpoint{}, #sip_message{}) -> {ok, sip_transport:connection()}.
send_request(Conn, To, Msg) ->
	sip_transport:send_request(Conn, To, Msg).

%% @doc
%% Send the response to the transport layer.
%% @end
-spec send_response(sip_transport:connection() | undefined, #sip_message{}) -> {ok, sip_transport:connection()}.
send_response(Conn, Msg) ->
	sip_transport:send_response(Conn, Msg).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
