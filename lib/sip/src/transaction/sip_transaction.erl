%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Transaction layer API. Implements transport layer handler
%%% behaviour.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction).

-behaviour(gen_server).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").
-include_lib("sip_transaction.hrl").

%% API
-export([start_link/0]).
-export([start_tx/5, list_tx/0]).
-export([handle/3, send/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Macros
-define(SERVER, ?MODULE).
-define(TX_SUP(Name, TxModule), ?SPEC(Name, sip_transaction_tx_sup, supervisor, [TxModule])).

%% Types
-record(state, {transactions = dict:new(), % Key -> pid()
                pids = dict:new()}).       % pid() -> Key

-type tx_key() :: {client, Branch :: binary(), Method :: sip_message:method()} | 
                  {server, SentBy :: sip_headers:via_sent_by(), Branch :: binary(), Method :: sip_message:method()}.
-type tx_ref() :: {tx_key(), pid()}.
-export_type([tx_key/0, tx_ref/0]).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @doc
%% Start new client or server transaction.
%% @end
-spec start_tx(client | server, any(), sip_transport:connection(), #conn_key{}, #sip_message{}) -> {ok, tx_ref()}.
start_tx(Kind, TU, Connection, Remote, Request)
  when (Kind =:= client orelse Kind =:= server),
       is_pid(TU),
       is_record(Request, sip_message),
       is_record(Remote, conn_key) ->
    
    Key = tx_key(Kind, Request),
    Module = tx_module(Kind, Request),
    Params = #params{connection = Connection,
                     key = Key,
                     remote = Remote,
                     tx_user = TU,
                     request = Request},
    gen_server:call(?SERVER, {start_tx, Module, Params}).

-spec list_tx() -> [tx_ref()].
list_tx() ->
    gen_server:call(?SERVER, list_tx).

%% @doc
%% Handle the given request/response on the transaction layer. Returns not_handled
%% if no transaction to handle the message is found.
%% @end
-spec handle(sip_transport:connection(), #conn_key{}, #sip_message{}) -> not_handled | {ok, tx_ref()}.
handle(_Connection, Remote, Msg)
  when is_record(Remote, conn_key),
       is_record(Msg, sip_message) ->

    % requests go to server transactions, responses go to client
    Kind = case sip_message:is_request(Msg) of
               true ->  server;
               false -> client
           end,
    % lookup transaction by key
    TxKey = tx_key(Kind, Msg),
    case lookup_tx(TxKey) of
        undefined -> not_handled;
        TxRef -> tx_send(TxRef, Msg)
    end.

%% @doc
%% Pass given message from the TU to the given transaction.
%% @end
-spec send(tx_ref(), #sip_message{}) -> not_handled | {ok, tx_ref()}.
send(TxRef, Msg)  when is_record(Msg, sip_message) ->
    tx_send(TxRef, Msg).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    {ok, #state{}}.

%% @private
-spec handle_call(_, _, #state{}) ->
          {reply, [sip_transaction:tx_ref()], #state{}} |
          {reply, {ok, sip_transaction:tx_ref()} | error, #state{}} |
          {reply, {ok, sip_transaction:tx_ref()}, #state{}} |
          {stop, {unexpected, _}, #state{}}.
handle_call(list_tx, _From, State) ->
    Result = dict:to_list(State#state.transactions),
    {reply, Result, State};

handle_call({lookup_tx, Key}, _From, State) ->
    Result = case dict:find(Key, State#state.transactions) of
                 {ok, Pid} -> {ok, {Key, Pid}};
                 error -> error
             end,
    {reply, Result, State};

handle_call({start_tx, Module, Params}, _From, State) ->   
    {ok, Pid} = sip_transaction_tx_sup:start_tx(Module, Params),
    Key = Params#params.key,

    % monitor transaction process
    erlang:monitor(process, Pid),
    Transactions = dict:store(Key, Pid, State#state.transactions),
    Pids = dict:store(Pid, Key, State#state.pids),
    {reply, {ok, {Key, Pid}}, State#state{transactions = Transactions, pids = Pids}};

handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {noreply, #state{}} | {stop, {unexpected, _}, #state{}}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    % clear pid -> key mapping
    Key = dict:fetch(Pid, State#state.pids),
    Pids = dict:erase(Pid, State#state.pids),

    % remove from the key -> pid mapping
    Transactions = dict:erase(Key, State#state.transactions),
    {noreply, State#state{transactions = Transactions, pids = Pids}};

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

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
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
                    {'cseq', CSeq} = lists:keyfind('cseq', 1, Headers),
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
        <<"z9hG4bK", _/binary>> = Branch ->
            Via = sip_headers:top_via(Headers),
            SentBy = Via#sip_hdr_via.sent_by,
            {server, SentBy, Branch, Method};

        % No branch or does not start with magic cookie
        _ ->
            % FIXME: use procedure from 17.2.3
            undefined
    end.

lookup_tx(Key) ->
    % RFC 17.2.3
    case Key of
        undefined -> undefined;
        _ ->
            case gen_server:call(?SERVER, {lookup_tx, Key}) of
                {ok, Pid} -> Pid;
                _ -> undefined
            end
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

tx_send(TxRef, Msg) when is_record(Msg, sip_message) ->

    {Kind, Param, _} = Msg#sip_message.start_line,
    % RFC 17.1.3/17.2.3
    case TxRef of
        % no transaction
        undefined ->
            not_handled;

        {_Key, Pid} ->
            ok = try gen_fsm:sync_send_event(Pid, {Kind, Param, Msg})
                 catch error:noproc -> not_handled % no transaction to handle
                 end,
            {ok, TxRef}
    end.
