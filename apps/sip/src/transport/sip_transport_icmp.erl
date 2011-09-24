%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Experimental module that captures ICMP reports and caches unreachable
%%% destinations for some time (1 minute by default). If destination address is
%%% known to be unreachable, an error is immediately returned to the transport user.
%%%
%%% Uses `gen_icmp' library to create ICMP socket.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_transport_icmp).

-behaviour(gen_server).

%% Exports

%% API
-export([start_link/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Macros
-define(SERVER, ?MODULE).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").
-include_lib("pkt/include/pkt.hrl").

-record(state, {socket}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, Socket} = gen_icmp:open(),
    {ok, #state{socket = Socket}}.

%% @private
-spec handle_call(_, _, #state{}) -> {reply, {term(), integer()}, #state{}} | {stop, _, #state{}}.
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info({icmp, _Socket, _Address, <<?ICMP_DEST_UNREACH:8, _Rest/binary>> = Packet}, State) ->
    {_ICMP, Rest} = pkt:icmp(Packet),
    {IPV4, Rest2} = pkt:ipv4(Rest),
    case IPV4#ipv4.p of
        ?IPPROTO_UDP ->
            {UDP, _Rest3} = pkt:udp(Rest2),
            % Notify about UDP destination being unreachable.
            % FIXME: More clean API. Probably, sip_transport:send_request/send_response should have an
            % option to send message back to the caller in case of transport layer errors.
            gproc:send({p, l, {udp_destination, IPV4#ipv4.daddr, UDP#udp.dport}}, {error, econnrefused}),
            {noreply, State};
        _Other ->
            {noreply, State}
    end;

handle_info({icmp, _Socket, _Address, _Packet}, State) ->
    % Ignore other ICMP messages
    {noreply, State};

handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_cast(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_cast(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
