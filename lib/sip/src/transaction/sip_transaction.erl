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

%% API
-export([start_link/1]).
-export([start_tx/4, list_tx/0]).
-export([handle/3, send/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Macros
-define(SERVER, ?MODULE).
-define(TX_SUP(Name, TxModule), ?SPEC(Name, sip_transaction_tx_sup, supervisor, [TxModule])).

%% Types
-record(state, {config,
				transactions = dict:new(), % Key -> pid()
				pids = dict:new()}).       % pid() -> Key

-type tx_key() :: {client, Branch :: binary(), Method :: sip_message:method()}.
-type tx_ref() :: {tx_key(), pid()}.
-export_type([tx_key/0, tx_ref/0]).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link(sip_config:config()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Cfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Cfg, []).

%% @doc
%% Start new client transaction.
%% @end 
-spec start_tx(client | server, any(), #sip_endpoint{}, #sip_message{}) -> {ok, tx_ref()}.
start_tx(Kind, TU, Remote, Request) 
  when (Kind =:= client orelse Kind =:= server), 
  	   is_pid(TU),
	   is_record(Request, sip_message),
	   is_record(Remote, sip_endpoint) ->
	gen_server:call(?SERVER, {start_tx, Kind, TU, Remote, Request}).

-spec list_tx() -> [tx_ref()].
list_tx() ->
	gen_server:call(?SERVER, list_tx).

%% @doc
%% Handle the given request/response on the transaction layer. Returns false
%% if no transaction to handle the message is found.
%% @end
-spec handle(sip_transport:connection(), #sip_endpoint{}, #sip_message{}) -> false | ok.
handle(_Connection, Remote, Msg) 
  when is_record(Remote, sip_endpoint), 
	   is_record(Msg, sip_message) ->
	
	% requests go to server transactions, responses go to client
	Kind = case sip_message:is_request(Msg) of
			   true ->
				   server;
			   
			   false ->
				   client
		   end,
	% lookup transaction by key
	TxRef = lookup_tx(tx_key(Kind, Msg)),
	case tx_send(TxRef, Msg) of		
		false when Kind =:= server ->
			% start server transaction if existing tx not found
			sip_transaction:start_tx(server, whereis(sip_core), Remote, Msg);
		
		Res ->
			Res
	end.

%% @doc
%% Pass given message from the TU to the given transaction.
%% @end
-spec send(tx_ref(), #sip_message{}) -> false | ok.
send(TxRef, Msg)  when is_record(Msg, sip_message) ->
	tx_send(TxRef, Msg).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({sip_config:config(), pid()}) -> {ok, #state{}}.
init(Cfg) ->
	{ok, #state{config = Cfg}}.

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

handle_call({start_tx, Kind, TU, Remote, Msg}, _From, State) ->
	Key = tx_key(Kind, Msg),
	Module = tx_module(Kind, Msg),
	
	{ok, Pid} = sip_transaction_tx_sup:start_tx({Key, Module}, TU, {Remote, Msg}),
	
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
		false ->
			false;
		
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
			false
	end.

lookup_tx(Key) ->
	% RFC 17.2.3
	case Key of
		false -> false;
		_ ->
			case gen_server:call(?SERVER, {lookup_tx, Key}) of
				{ok, Pid} -> Pid;
				_ -> false
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
		false ->
			false;
		
		{_Key, Pid} ->
			try gen_fsm:sync_send_event(Pid, {Kind, Param, Msg})
			catch error:noproc -> false % no transaction to handle
			end
	end.
