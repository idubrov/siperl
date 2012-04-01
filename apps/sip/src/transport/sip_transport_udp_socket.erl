%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UDP SIP socket implementation.
%%%
%%% <em>Note: transport is responsible for basic validation of
%%% incoming requests/responses and sending "400 Bad Request/Response".</em>
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------,
-module(sip_transport_udp_socket).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------

%% API
-export([send/3]).

%% Server callbacks
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip.hrl").

-record(state, {socket, error, timeout = infinity :: integer() | infinity}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------


%% @private
%% @doc
%% Send SIP message through the given socket.
%% @end
-spec send(pid(), #sip_destination{}, sip_message()) -> ok | {error, Reason :: term()}.
send(Pid, To, Message) when
  is_pid(Pid),
  is_record(To, sip_destination),
  (is_record(Message, sip_request) orelse is_record(Message, sip_response)) ->
    gen_server:call(Pid, {send, To, Message}).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec start_link(integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Port) when is_integer(Port) ->
    gen_server:start_link(?MODULE, Port, []).

%% @private
-spec init(integer()) -> {ok, #state{}}.
init(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, inet, {reuseaddr, true}]),
    % used for looking up socket
    true = gproc:add_local_name({udp, Port}),
    {ok, #state{socket = Socket}}.

%% @private
-spec handle_info({udp, inet:socket(), inet:address(), integer(), binary()} | term(), #state{}) ->
          {noreply, #state{}, infinity} | {stop, _Reason, #state{}}.
handle_info({udp, Socket, SrcAddress, SrcPort, Packet}, State) ->
    Remote = #sip_destination{transport = udp, address = SrcAddress, port = SrcPort},
    case sip_message:parse_datagram(Packet) of
        {ok, Msg} ->
            case validate(Msg) of
                ok ->
                    ok = sip_transport:dispatch(Remote, Msg);
                {error, Reason} ->
                    bad_message(Msg, Reason, Remote, Socket)
            end;
        {error, Reason, Msg} ->
            bad_message(Msg, Reason, Remote, Socket)
    end,
    {noreply, State, State#state.timeout};
handle_info({udp_error, _Socket, Reason}, State) ->
    {noreply, State#state{error = Reason}, State#state.timeout};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(Req, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_call({send, #sip_destination{}, sip_message()}, _, #state{}) ->
          {reply, {error, too_big}, #state{}, integer() | infinity} |
          {reply, {ok, #sip_destination{}}, #state{}, integer() | infinity}.
handle_call(_Req, _From, #state{error = Reason} = State)
  when Reason =/= undefined ->
    {reply, {error, Reason}, State#state{error = undefined}, State#state.timeout};
handle_call({send, To, Message}, _From, State) ->
    Packet = sip_message:to_binary(Message),
    %% If a request is within 200 bytes of the path MTU, or if it is larger
    %% than 1300 bytes and the path MTU is unknown, the request MUST be sent
    %% using an RFC 2914 [43] congestion controlled transport protocol, such
    %% as TCP.
    Reply =
        if size(Packet) > 1300 ->
               {error, too_big};
           true ->
               ok = gen_udp:send(State#state.socket, To#sip_destination.address, To#sip_destination.port, Packet)
        end,
    {reply, Reply, State, State#state.timeout};

handle_call(Req, _From, State) ->
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


validate(#sip_request{} = Request) -> sip_message:validate_request(Request);
validate(#sip_response{} = Response) -> sip_message:validate_response(Response).

bad_message(#sip_request{} = Request, Reason, Remote, Socket) ->
    sip_log:bad_request(Request, Reason, Remote),

    % TODO: Put reason in the response
    Response = sip_message:create_response(Request, 400),
    Packet = sip_message:to_binary(Response),

    % Don't bother about return value
    _Ignore = gen_udp:send(Socket,
                           Remote#sip_destination.address,
                           Remote#sip_destination.port,
                           Packet),
    ok;
bad_message(#sip_response{} = Response, Reason, Remote, _Socket) ->
    sip_log:bad_response(Response, Reason, Remote),
    ok.
