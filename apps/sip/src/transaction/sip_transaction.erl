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

% Client API
-export([start_client_tx/3, start_client_tx/4, start_server_tx/2, send_response/1, tx_key/2]).
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

    % Every new client transaction has its own branch value
    Request2 = sip_message:with_branch(sip_idgen:generate_branch(), Request),

    % XXX: Note that request could be sent via TCP instead of UDP due to the body being oversized
    Reliable = sip_transport:is_reliable(To#sip_destination.transport),

    Key = tx_key(client, Request2),
    Module = tx_module(client, Request2),
    TxState = #tx_state{to = To,
                        tx_key = Key,
                        tx_user = TU,
                        request = Request2,
                        reliable = Reliable,
                        options = Options},
    {ok, _Pid} = sip_transaction_tx_sup:start_tx(Module, TxState),
    {ok, Key}.

%% @doc
%% Start new server transaction.
%% @end
-spec start_server_tx(term(), #sip_request{}) -> {ok, #sip_tx_server{}}.
start_server_tx(TU, Request)
  when is_record(Request, sip_request) ->

    % Check top via in received request to check transport reliability
    Via = sip_message:header_top_value('via', Request),
    Reliable = sip_transport:is_reliable(Via#sip_hdr_via.transport),

    Key = tx_key(server, Request),
    Module = tx_module(server, Request),
    TxState = #tx_state{tx_key = Key,
                        request = Request,
                        tx_user = TU,
                        reliable = Reliable},
    {ok, _Pid} = sip_transaction_tx_sup:start_tx(Module, TxState),
    {ok, Key}.

-spec list_tx() -> [#sip_tx_client{} | #sip_tx_server{}].
list_tx() ->
    gproc:select(names,
                 [{{'$1','$2','$3'},
                   % Match {n, _, {tx, Key}}
                   [{'=:=', tx, {element, 1, {element, 3, '$1'}}}],
                   % Return {Key, Pid}
                   [{element, 2, {element, 3, '$1'}}]}]).

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
    tx_send(Key, Msg).

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

tx_send(Key, Msg) ->
    {Kind, Param} =
        case Msg of
            #sip_request{method = Method} -> {request, Method};
            #sip_response{status = Status} -> {response, Status}
        end,
    % RFC 17.1.3/17.2.3
    case gproc:lookup_local_name({tx, Key}) of
        % no transaction
        undefined ->
            not_handled;

        Pid when is_pid(Pid) ->
            ok = try gen_fsm:sync_send_event(Pid, {Kind, Param, Msg})
                 catch error:noproc -> not_handled % no transaction to handle
                 end,
            {ok, Key}
    end.
