%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction layer API. Implements transport layer handler
%%% behaviour.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip.hrl").
-include_lib("sip_transaction.hrl").

% Client API
-export([start_client_tx/3, start_server_tx/3, send_response/2]).
% Management API
-export([list_tx/0]).
% API for transport layer
-export([handle_request/1, handle_response/1]).

%% Macros
-define(SERVER, ?MODULE).
-define(TX_SUP(Name, TxModule), ?SPEC(Name, sip_transaction_tx_sup, supervisor, [TxModule])).

%% Types
-type tx_key() :: {client, Branch :: binary(), Method :: sip_message:method()} |
                  {server, SentBy :: sip_headers:via_sent_by(), Branch :: binary(), Method :: sip_message:method()}.
-export_type([tx_key/0]).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
%% @doc
%% Start new client transaction.
%% @end
-spec start_client_tx(pid() | term(), #sip_destination{}, #sip_message{}) -> {ok, tx_key()}.
start_client_tx(TU, To, Request)
  when is_record(To, sip_destination),
       is_record(Request, sip_message) ->

    Key = tx_key(client, Request),
    Module = tx_module(client, Request),
    TxState = #tx_state{to = To,
                        tx_key = Key,
                        tx_user = TU,
                        request = Request},
    {ok, _Pid} = sip_transaction_tx_sup:start_tx(Module, TxState),
    {ok, Key}.

%% @doc
%% Start new server transaction.
%% @end
-spec start_server_tx(pid() | term(), sip_transport:connection(), #sip_message{}) -> {ok, tx_key()}.
start_server_tx(TU, Connection, Request)
  when is_record(Request, sip_message) ->

    Key = tx_key(server, Request),
    Module = tx_module(server, Request),
    TxState = #tx_state{connection = Connection,
                        tx_key = Key,
                        tx_user = TU,
                        request = Request},
    {ok, _Pid} = sip_transaction_tx_sup:start_tx(Module, TxState),
    {ok, Key}.

-spec list_tx() -> [tx_key()].
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
-spec handle_request(#sip_message{}) -> not_handled | {ok, tx_key()}.
handle_request(Msg) when is_record(Msg, sip_message) ->
    true = sip_message:is_request(Msg),
    handle_internal(server, Msg).

%% @doc
%% Handle the given response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
%% @private
-spec handle_response(#sip_message{}) -> not_handled | {ok, tx_key()}.
handle_response(Msg) ->
    true = sip_message:is_response(Msg),
    handle_internal(client, Msg).

%% @doc
%% Pass given response from the TU to the given transaction.
%% @end
-spec send_response(tx_key(), #sip_message{}) -> not_handled | {ok, tx_key()}.
send_response(Key, Msg) ->
    true = sip_message:is_response(Msg),
    tx_send(Key, Msg).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc
%% Handle the given request/response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
handle_internal(Kind, Msg) when is_record(Msg, sip_message) ->
    % lookup transaction by key
    Key = tx_key(Kind, Msg),
    tx_send(Key, Msg).

tx_key(client, Msg) ->
    % RFC 17.1.3
    Headers = Msg#sip_message.headers,
    case sip_headers:top_via_branch(Headers) of
        % no branch parameter
        undefined ->
            undefined;

        Branch ->
            case Msg#sip_message.start_line of
                {request, Method, _} ->
                    {client, Branch, Method};

                {response, _, _} ->
                    {ok, CSeq} = sip_headers:top_header('cseq', Headers),
                    Method = CSeq#sip_hdr_cseq.method,
                    {client, Branch, Method}
            end
    end;

tx_key(server, Request) ->
    % RFC 17.2.3
    Headers = Request#sip_message.headers,

    % for ACK we use INVITE
    Method = case Request#sip_message.start_line of
                 {request, 'ACK', _} -> 'INVITE';
                 {request, M, _} -> M
             end,
    case sip_headers:top_via_branch(Headers) of
        % Magic cookie
        <<?MAGIC_COOKIE, _/binary>> = Branch ->
            {ok, Via} = sip_headers:top_header('via', Headers),
            SentBy = Via#sip_hdr_via.sent_by,
            {server, SentBy, Branch, Method};

        % No branch or does not start with magic cookie
        _ ->
            % FIXME: use procedure from 17.2.3
            undefined
    end.

%% @doc
%% Get transaction key and module.
%% @end
tx_module(client, Request) ->
    case Request#sip_message.start_line of
        {request, 'INVITE', _} -> sip_transaction_client_invite;
        {request, _, _} -> sip_transaction_client
    end;

%% @doc
%% Get transaction key and module.
%% @end
tx_module(server, Request) ->
    case Request#sip_message.start_line of
        {request, 'INVITE', _} -> sip_transaction_server_invite;
        {request, 'ACK', _} -> sip_transaction_server_invite;
        {request, _, _} -> sip_transaction_server
     end.

tx_send(undefined, _Msg) ->
    not_handled;
tx_send(Key, Msg) when is_record(Msg, sip_message) ->

    {Kind, Param, _} = Msg#sip_message.start_line,
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
