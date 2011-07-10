%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Base transaction functions. Used in macroses froms
%%% sip_transaction.hrl to simplify transactions FSM code.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_transaction_base).

%% Include files
-include_lib("../sip_common.hrl").
-include_lib("sip_transaction.hrl").
-include_lib("sip.hrl").

%% Exports

%% FSM callbacks
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
-export([start_timer/4, cancel_timer/2]).
-export([send_ack/2, send_request/2, send_response/2, init/1, pass_to_tu/2]).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------

-spec init(#params{}) -> #data{}.
init(#params{to = To, connection = Connection, key = Key, request = Request, tx_user = TxUser})
  when is_record(Request, sip_message) ->

    % If message was received via reliable connection
    Via = sip_headers:top_via(Request#sip_message.headers),
    Reliable = sip_transport:is_reliable(Via#sip_hdr_via.transport),
    
    % Register transaction under its key
    gproc:add_local_name({tx, Key}),

    % start monitoring TU user so we terminate if it does
    monitor(process, TxUser),
    #data{t1 = sip_config:t1(),
          t2 = sip_config:t2(),
          t4 = sip_config:t4(),
          to = To,
          connection = Connection,
          tx_user = TxUser,
          request = Request,
          reliable = Reliable,
          tx_ref = {Key, self()}}.

-spec cancel_timer(integer(), #data{}) -> #data{}.
cancel_timer(TimerIdx, Data)
  when is_integer(TimerIdx), is_record(Data, data) ->
    case element(TimerIdx, Data) of
        undefined ->
            Data;

        Timer ->
            gen_fsm:cancel_timer(Timer),
            setelement(TimerIdx, Data, undefined)
    end.

-spec start_timer(atom(), integer(), integer(), #data{}) -> #data{}.
start_timer(TimerName, TimerIdx, Interval, Data) ->
    Timer = gen_fsm:start_timer(Interval, {TimerName, Interval}),
    setelement(TimerIdx, Data, Timer).

-spec send_ack(#sip_message{}, #data{}) -> #data{}.
send_ack(Response, Data) ->
    ACK = sip_message:create_ack(Data#data.request, Response),
    send_request(ACK, Data).

-spec send_request(#sip_message{}, #data{}) -> #data{}.
send_request(Msg, Data) ->
    % Send request to the given destination address
    sip_transport:send_request(Data#data.to, Msg, []),
    Data.

-spec send_response(#sip_message{}, #data{}) -> #data{}.
send_response(Msg, Data) ->
    % Send response using the connection of original request
    sip_transport:send_response(Data#data.connection, Msg),
    Data.

-spec pass_to_tu(#sip_message{}, #data{}) -> term().
pass_to_tu(Msg, Data) ->
    {Kind, _, _} = Msg#sip_message.start_line,
    TU = Data#data.tx_user,
    TU ! {tx, Data#data.tx_ref, {Kind, Msg}},
    Data.

%% @private
-spec handle_event(term(), atom(), #data{}) ->
          {stop, term(), #data{}}.
handle_event(Event, _State, Data) ->
    {stop, {unexpected, Event}, Data}.

%% @private
-spec handle_sync_event(term(), term(), atom(), #data{}) ->
          {stop, term(), term(), #data{}}.
handle_sync_event(Event, _From, _State, Data) ->
    Reason = {unexpected, Event},
    {stop, Reason, Reason, Data}.

%% @private
-spec handle_info(term(), atom(), #data{}) ->
          {stop, term(), #data{}}.
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State, Data) ->
    % we mostly ignore when TU is down, it is only handled in server
    % transactions when response from TU is expected
    {next_state, State, Data};
handle_info(Info, _State, Data) ->
    {stop, {unexpected, Info}, Data}.

%% @doc
%% Inform the transaction user about transition to 'TERMINATED' state.
%% @end
-spec terminate(term(), atom(), #data{}) -> ok.
terminate(Reason, _State, Data) ->
    TU = Data#data.tx_user,
    TU ! {tx, Data#data.tx_ref, {terminated, Reason}},
    ok.

%% @private
-spec code_change(term(), atom(), #data{}, term()) -> {ok, atom(), #data{}}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

