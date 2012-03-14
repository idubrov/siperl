%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Transaction layer API. Implements transport layer handler
%%% behaviour.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transaction).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").
-include("sip_transaction.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

% Client API
-export([start_client_tx/3, start_server_tx/2, send_response/1, tx_key/2, cancel/1]).
-export([list_tx/0, is_loop_detected/1]).

% Internal API for transport layer
-export([handle_request/1, handle_response/1]).

%% Macros
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc Start new client transaction
%%
%% `tu' is pid of the process to report progress to. If `none' is specified,
%% transaction will not report its progress (fire-and-forget transaction).
%% By default, current process is used.
%% @end
-spec start_client_tx(#sip_destination{},
                      #sip_request{},
                      [{ttl, non_neg_integer()} | {tu, pid() | none}]) -> {ok, pid()}.
start_client_tx(Destination, Request, Options)
  when is_record(Destination, sip_destination),
       is_record(Request, sip_request),
       is_list(Options) ->

    % XXX: Note that request could be sent via TCP instead of UDP due to the body being oversized
    Reliable = sip_transport:is_reliable(Destination#sip_destination.transport),

    TxState = #tx_state{reliable = Reliable},
    do_start_tx(client, Request, Options, TxState, Destination).

%% @doc Start new server transaction
%%
%% `tu' is pid of the process to report progress to. If `none' is specified,
%% transaction will not report its progress (fire-and-forget transaction).
%% By default, current process is used.
%% @end
-spec start_server_tx(#sip_request{}, []) -> {ok, pid()}.
start_server_tx(Request, Options)
  when is_record(Request, sip_request),
       is_list(Options) ->

    % Check top via in received request to check transport reliability
    Via = sip_message:header_top_value(via, Request),
    Reliable = sip_transport:is_reliable(Via#sip_hdr_via.transport),

    TxState = #tx_state{reliable = Reliable},
    do_start_tx(server, Request, Options, TxState, undefined).

%% Start transaction process
do_start_tx(Kind, Request, Options, TxState, Destination) ->
    TU = proplists:get_value(tu, Options, self()), % Default TU is self
    TxKey = tx_key(Kind, Request),
    Module = tx_module(Kind, Request),
    TxState2 = TxState#tx_state{destination = Destination,
                                tx_key = TxKey,
                                tx_user = TU,
                                request = Request,
                                options = Options},

    {ok, Pid} = sip_transaction_tx_sup:start_tx(Module, TxKey),
    ok = gen_fsm:send_event(Pid, {init, TxState2}),
    {ok, Pid}.

-spec list_tx() -> [#sip_tx_client{} | #sip_tx_server{}].
list_tx() ->
    MS = ets:fun2ms(fun ({{n, l, {tx, TxKey}}, _Pid, _Value}) -> TxKey end),
    gproc:select(names, MS).

-spec cancel(#sip_request{}) -> {ok, pid()} | false.
%% @doc Cancel server transaction based on the `CANCEL' request
%%
%% This functionality was moved from the UAS here, because transaction
%% layer have the information if request was replied and can process
%% cancellation atomically (in transaction FSM process).
%% @end
cancel(#sip_request{method = 'CANCEL'} = Request) ->
    Key = tx_key(server, Request),
    % lookup by the key, but with method not CANCEL/ACK
    MS = ets:fun2ms(fun ({{n, l, {tx, TxKey}}, Pid, _Value})
                          when is_record(TxKey, sip_tx_server),
                               TxKey#sip_tx_server.branch =:= Key#sip_tx_server.branch,
                               TxKey#sip_tx_server.host =:= Key#sip_tx_server.host,
                               TxKey#sip_tx_server.port =:= Key#sip_tx_server.port,
                               TxKey#sip_tx_server.method =/= 'CANCEL', TxKey#sip_tx_server.method =/= 'ACK'
                          -> Pid end),
    case gproc:select(names, MS) of
        [Pid] ->
            ok = gen_fsm:send_event(Pid, cancel),
            {ok, Pid};
        [] -> false
    end.

%% @doc
%% Handle the given request on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
%% @private
-spec handle_request(#sip_request{}) -> not_handled | {ok, pid()}.
handle_request(Msg) when is_record(Msg, sip_request) ->
    handle_internal(server, Msg).

%% @doc
%% Handle the given response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
%% @private
-spec handle_response(#sip_response{}) -> not_handled | {ok, pid()}.
handle_response(Msg) ->
    handle_internal(client, Msg).

%% @doc Pass given response from the TU to the server transaction.
%% @end
-spec send_response(#sip_response{}) -> {ok, pid()}.
send_response(Msg) ->
    handle_internal(server, Msg).

%% @doc Check message against loop conditions
%%
%% Check if loop is detected by by following procedures from 8.2.2.2
%% @end
-spec is_loop_detected(#sip_request{}) -> boolean().
is_loop_detected(#sip_request{} = Request) ->
    sip_transaction_base:is_loop_detected(Request).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc
%% Handle the given request/response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
handle_internal(Kind, Msg) ->
    % lookup transaction by key
    Key = tx_key(Kind, Msg),
    case Msg of
        #sip_request{method = Method} -> tx_send(Key, {request, Method, Msg});
        #sip_response{status = Status} -> tx_send(Key, {response, Status, Msg})
    end.

%% @doc Determine transaction unique key
%% @end
-spec tx_key(client | server, sip_message()) -> #sip_tx_client{} | #sip_tx_server{}.
tx_key(client, Msg) ->
    % RFC 17.1.3
    Method = sip_message:method(Msg),
    Via = sip_message:header_top_value(via, Msg),
    {branch, Branch} = lists:keyfind(branch, 1, Via#sip_hdr_via.params),
    #sip_tx_client{branch = Branch, method = Method};
tx_key(server, Msg) ->
    % RFC 17.2.3
    % for ACK we use INVITE
    Method =
        case sip_message:method(Msg) of
            'ACK' -> 'INVITE';
            M -> M
        end,
    Via = sip_message:header_top_value(via, Msg),
    case lists:keyfind(branch, 1, Via#sip_hdr_via.params) of
        % Magic cookie
        {branch, <<?MAGIC_COOKIE, _/binary>> = Branch} ->
            Via = sip_message:header_top_value('via', Msg),
            Host = Via#sip_hdr_via.host,
            Port = Via#sip_hdr_via.port,
            #sip_tx_server{host = Host, port = Port, branch = Branch, method = Method}

        % No branch or does not start with magic cookie
        %_ ->
        %    % FIXME: use procedure from 17.2.3
        %    undefined
    end.

%% @doc
%% Get transaction module based on the transaction type and method.
%% @end
tx_module(client, #sip_request{method = 'INVITE'}) ->sip_transaction_client_invite;
tx_module(client, #sip_request{}) -> sip_transaction_client;
tx_module(server, #sip_request{method = 'INVITE'}) -> sip_transaction_server_invite;
tx_module(server, #sip_request{method = 'ACK'}) -> sip_transaction_server_invite;
tx_module(server, #sip_request{}) -> sip_transaction_server.

tx_send(Key, Info) ->
    % RFC 17.1.3/17.2.3
    case gproc:lookup_local_name({tx, Key}) of
        % no transaction
        undefined ->
            not_handled;

        Pid when is_pid(Pid) ->
            ok = gen_fsm:send_event(Pid, Info),
            {ok, Pid}
    end.
