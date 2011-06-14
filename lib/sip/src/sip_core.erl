%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP application core.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_core).

-behaviour(gen_server).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip_message.hrl").
-include_lib("sip_transport.hrl").

%% API
-export([start_link/1]).
-export([handle/3]).

%% Macros
-define(SERVER, ?MODULE).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Types
-record(state, {config}).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
-spec start_link(sip_config:config()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Cfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Cfg, []).

-spec handle(sip_transport:connection(), #sip_endpoint{}, #sip_message{}) -> ok.
handle(_Connection, Remote, Msg) 
  when is_record(Remote, sip_endpoint), 
	   is_record(Msg, sip_message) ->
	
	case sip_message:is_request(Msg) of
		true ->
			sip_transaction:start_tx(server, whereis(sip_core), Remote, Msg),
			ok;
		
		false ->
			ok
	end.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({sip_config:config(), pid()}) -> {ok, #state{}}.
init(Cfg) ->
	{ok, #state{config = Cfg}}.

%% @private
-spec handle_call(_, _, #state{}) -> 
		  {stop, {unexpected, _}, #state{}}.
handle_call(Req, _From, State) ->
    {stop, {unexpected, Req}, State}.

%% @private
-spec handle_info(_, #state{}) -> {stop, {unexpected, _}, #state{}}.
handle_info({tx, {_, Pid}, {request, Request}}, State) ->
	%% send response...
	%?debugFmt("Got message from ~p data ~p~n", [Pid, Data]),
	Response = sip_message:create_response(Request, 486, <<"Busy here">>, <<"anytag">>),
	gen_fsm:sync_send_event(Pid, {response, 486, Response}),
	{noreply, State};
handle_info({tx, _, _TxReq}, State) ->
	% ignore TX messages for now..
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
