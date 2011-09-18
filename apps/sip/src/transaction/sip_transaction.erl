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
-export([start_client_tx/3, start_client_tx/4, start_server_tx/2, send_response/1, tx_key/2, cancel/1]).
-export([list_tx/0, is_loop_detected/1]).

% Internal API for transport layer
-export([handle_request/1, handle_response/1]).

%% Macros
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
%% @doc Start new client transaction.
%% @end
-spec start_client_tx(pid() | term(), #sip_destination{}, #sip_request{}) -> {ok, #sip_tx_client{}}.
start_client_tx(TU, To, Request) ->
  start_client_tx(TU, To, Request, []).

%% @doc Start new client transaction with user data associated with it.
%% @end
-spec start_client_tx(pid() | term(),
                      #sip_destination{},
                      #sip_request{},
                      [{ttl, non_neg_integer()} | {user_data, any()}]) -> {ok, #sip_tx_client{}}.
start_client_tx(TU, To, Request, Options)
  when is_record(To, sip_destination),
       is_record(Request, sip_request),
       is_list(Options) ->

    % XXX: Note that request could be sent via TCP instead of UDP due to the body being oversized
    Reliable = sip_transport:is_reliable(To#sip_destination.transport),

    TxKey = tx_key(client, Request),
    Module = tx_module(client, Request),
    TxState = #tx_state{to = To,
                        tx_key = TxKey,
                        tx_user = TU,
                        request = Request,
                        reliable = Reliable,
                        options = Options,
                        props = tx_props(client, TxKey, Request)},
    {ok, Pid} = sip_transaction_tx_sup:start_tx(Module, TxKey),
    ok = gen_fsm:sync_send_event(Pid, {init, TxState}),
    {ok, TxKey}.

%% @doc
%% Start new server transaction.
%% @end
-spec start_server_tx(term(), #sip_request{}) -> {ok, #sip_tx_server{}}.
start_server_tx(TU, Request)
  when is_record(Request, sip_request) ->

    % Check top via in received request to check transport reliability
    Via = sip_message:header_top_value('via', Request),
    Reliable = sip_transport:is_reliable(Via#sip_hdr_via.transport),

    TxKey = tx_key(server, Request),
    Module = tx_module(server, Request),
    TxState = #tx_state{tx_key = TxKey,
                        request = Request,
                        tx_user = TU,
                        reliable = Reliable,
                        props = tx_props(server, TxKey, Request)},
    {ok, Pid} = sip_transaction_tx_sup:start_tx(Module, TxKey),
    ok = gen_fsm:sync_send_event(Pid, {init, TxState}),
    {ok, TxKey}.


-spec list_tx() -> [#sip_tx_client{} | #sip_tx_server{}].
list_tx() ->
    MS = ets:fun2ms(fun ({{n, l, {tx, TxKey}}, _Pid, _Value}) -> TxKey end),
    gproc:select(names, MS).

-spec cancel(#sip_request{}) -> {ok, #sip_tx_server{}} | false;
            (#sip_tx_client{}) -> {ok, #sip_tx_client{}} | false.
%% @doc Cancel client or server transaction based on the request or transaction
%% key (see 9.1/9.2)
%%
%% If request is provided, cancel server transaction (request was received from
%% outside). The request must have 'CANCEL' method.
%%
%% If client transaction key was provided, cancel client transaction matching the
%% key. The client transaction will initiate `CANCEL' transactions on its own,
%% based on the conditions described in 9.1.
%%
%% This functionality was moved from the UAC/UAS here, because transaction
%% layer have the information if request was replied and can process
%% cancellation atomically (in transaction FSM process).
%% @end
cancel(#sip_request{method = 'CANCEL'} = Request) ->
    Key = tx_key(server, Request),
    % lookup by the key, but with method not CANCEL/ACK
    MS = ets:fun2ms(fun ({{n, l, {tx, TxKey}}, _Pid, _Value})
                          when is_record(TxKey, sip_tx_server),
                               TxKey#sip_tx_server.branch =:= Key#sip_tx_server.branch,
                               TxKey#sip_tx_server.host =:= Key#sip_tx_server.host,
                               TxKey#sip_tx_server.port =:= Key#sip_tx_server.port,
                               TxKey#sip_tx_server.method =/= 'CANCEL', TxKey#sip_tx_server.method =/= 'ACK'
                          -> TxKey end),
    case gproc:select(names, MS) of
        [TxKey] ->
            tx_send(TxKey, cancel),
            {ok, TxKey};
        [] -> false
    end;

cancel(TxKey) when is_record(TxKey, sip_tx_client) ->
    case tx_send(TxKey, cancel) of
        {ok, TxKey} -> {ok, TxKey};
        not_handled -> false % FIXME: use false instead of not_handled?
    end.


%% @doc
%% Handle the given request on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
%% @private
-spec handle_request(#sip_request{}) -> not_handled | {ok, #sip_tx_client{} | #sip_tx_server{}}.
handle_request(Msg) when is_record(Msg, sip_request) ->
    handle_internal(server, Msg).

%% @doc
%% Handle the given response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
%% @private
-spec handle_response(#sip_response{}) -> not_handled | {ok, #sip_tx_client{} | #sip_tx_server{}}.
handle_response(Msg) ->
    handle_internal(client, Msg).

%% @doc Pass given response from the TU to the server transaction.
%% @end
-spec send_response(#sip_response{}) -> {ok, #sip_tx_server{}}.
send_response(Msg) ->
    handle_internal(server, Msg).

%% @doc Check message against loop conditions
%%
%% Check if loop is detected by by following procedures from 8.2.2.2
%% @end
-spec is_loop_detected(#sip_request{}) -> boolean().
is_loop_detected(#sip_request{} = Msg) ->
    To = sip_message:header_top_value(to, Msg),

    case lists:keyfind(tag, 1, To#sip_hdr_address.params) of
        false ->
            TxKey = sip_transaction:tx_key(server, Msg),

            From = sip_message:header_top_value(from, Msg),
            {tag, FromTag} = lists:keyfind(tag, 1, From#sip_hdr_address.params),

            CallId = sip_message:header_top_value('call-id', Msg),
            CSeq = sip_message:header_top_value('cseq', Msg),
            List = gproc:lookup_local_properties({tx_loop, FromTag, CallId, CSeq}),
            case List of
                % either no transactions with same From: tag, Call-Id and CSeq
                % or there is one such transaction and message matches it
                [] -> false;
                [{_Pid, TxKey}] -> false;
                % there are transactions that have same From: tag, Call-Id and CSeq,
                % but message does not matches them --> loop detected
                _Other -> true
            end;
        % tag present, no loop
        {tag, _Tag} -> false
    end.

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
            ok = try gen_fsm:sync_send_event(Pid, Info)
                 catch error:noproc -> not_handled % no transaction to handle
                 end,
            {ok, Key}
    end.

tx_props(client, _TxKey, #sip_request{}) -> [];
tx_props(server, TxKey, #sip_request{} = Msg) ->
    % Add gproc: property for loop detection for server transactions, see 8.2.2.2
    From = sip_message:header_top_value(from, Msg),
    case lists:keyfind(tag, 1, From#sip_hdr_address.params) of
        {tag, FromTag} ->
            CallId = sip_message:header_top_value('call-id', Msg),
            CSeq = sip_message:header_top_value(cseq, Msg),
            Prop = {tx_loop, FromTag, CallId, CSeq},
            [{Prop, TxKey}];
        false ->
            []
    end.
