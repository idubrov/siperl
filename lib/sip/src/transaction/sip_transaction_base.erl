%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% Base transaction functions. Used in macroses froms
%%% sip_transaction.hrl to simplify transactions FSM code.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_transaction_base).

%% Include files
-include("../sip_common.hrl").
-include("sip_transaction.hrl").
-include("sip.hrl").

%% Exports


%% FSM callbacks
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
-export([start_timer/4, cancel_timer/2]).
-export([send_ack/2, send_request/2, send_response/2, init/1, pass_to_tu/2]).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec init(#tx_state{}) -> {ok, atom(), #tx_state{}}.
init(#tx_state{tx_key = Key, tx_user = TxUser, request = Msg} = TxState) ->
    % Register transaction under its key
    gproc:add_local_name({tx, Key}),

    % Add gproc: property for loop detection for server transactions, see 8.2.2.2
    case Key of
        #sip_tx_server{} ->
            case sip_message:tag('from', Msg) of
                {value, FromTag} ->
                    CallId = sip_message:top_header('call-id', Msg),
                    CSeq = sip_message:top_header('cseq', Msg),
                    gproc:add_local_property({tx_loop, FromTag, CallId, CSeq}, Key),
                    ok;
                false ->
                    ok
            end;
        #sip_tx_client{} ->
            ok
    end,

    % start monitoring TU user so we terminate if it does
    % FIXME: mechanism for detecting gproc-registered TUs failures
    case TxUser of
        Pid when is_pid(Pid) ->
            monitor(process, TxUser);
        _ -> ok
    end,
    % Send initialization message to ourselves, to make any heavy-weight
    % initialization. Also, any failure during the heavy-weight initialization
    % is appropriately reported to the TU (see 8.1.3.1, RFC 3261)
    gen_fsm:send_event(self(), init),
    {ok, 'INIT', TxState}.

-spec cancel_timer(integer(), #tx_state{}) -> #tx_state{}.
cancel_timer(TimerIdx, TxState)
  when is_integer(TimerIdx), is_record(TxState, tx_state) ->
    case element(TimerIdx, TxState) of
        undefined ->
            TxState;

        Timer ->
            gen_fsm:cancel_timer(Timer),
            setelement(TimerIdx, TxState, undefined)
    end.

-spec start_timer(atom(), integer(), integer(), #tx_state{}) -> #tx_state{}.
start_timer(TimerName, TimerIdx, Interval, TxState) ->
    Timer = gen_fsm:start_timer(Interval, {TimerName, Interval}),
    setelement(TimerIdx, TxState, Timer).

-spec send_ack(#sip_message{}, #tx_state{}) -> #tx_state{}.
send_ack(Response, TxState) ->
    ACK = sip_message:create_ack(TxState#tx_state.request, Response),
    send_request(ACK, TxState).

-spec send_request(#sip_message{}, #tx_state{}) -> #tx_state{}.
send_request(Msg, TxState) ->
    % Send request to the given destination address
    case sip_transport:send_request(TxState#tx_state.to, Msg, []) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end,
    TxState.

-spec send_response(#sip_message{}, #tx_state{}) -> #tx_state{}.
send_response(Msg, TxState) ->
    case sip_transport:send_response(Msg) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end,
    TxState.

-spec pass_to_tu(#sip_message{}, #tx_state{}) -> term().
pass_to_tu(Msg, TxState) ->
    Kind =
        case Msg#sip_message.kind of
            #sip_request{} -> request;
            #sip_response{} -> response
        end,
    notify_tu(TxState, {Kind, Msg}),
    TxState.

%% @private
-spec handle_event(term(), atom(), #tx_state{}) ->
          {stop, term(), #tx_state{}}.
handle_event(Event, _State, TxState) ->
    {stop, {unexpected, Event}, TxState}.

%% @private
-spec handle_sync_event(term(), term(), atom(), #tx_state{}) ->
          {stop, term(), term(), #tx_state{}}.
handle_sync_event(Event, _From, _State, TxState) ->
    Reason = {unexpected, Event},
    {stop, Reason, Reason, TxState}.

%% @private
-spec handle_info(term(), atom(), #tx_state{}) ->
          {stop, term(), #tx_state{}}.
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State, TxState) ->
    % we mostly ignore when TU is down, it is only handled in server
    % transactions when response from TU is expected
    {next_state, State, TxState};
handle_info(Info, _State, TxState) ->
    {stop, {unexpected, Info}, TxState}.

%% @doc
%% Inform the transaction user about transition to 'TERMINATED' state.
%% @end
-spec terminate(term(), atom(), #tx_state{}) -> ok.
terminate(Reason, _State, TxState) ->
    notify_tu(TxState, {tx, TxState#tx_state.tx_key, {terminated, Reason}}),
    ok.

%% @private
-spec code_change(term(), atom(), #tx_state{}, term()) -> {ok, atom(), #tx_state{}}.
code_change(_OldVsn, State, TxState, _Extra) ->
    {ok, State, TxState}.

notify_tu(TxState, Msg) ->
    case TxState#tx_state.tx_user of
        Pid when is_pid(Pid) -> Pid ! Msg;
        % Name must be gproc-registered name
        Name ->
            gproc:send({n, l, Name}, Msg)
    end,
    ok.

