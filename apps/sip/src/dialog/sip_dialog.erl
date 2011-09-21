%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog server
%%%
%%% Manages SIP dialog creation, state, updates and expiration.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_dialog).

%% API
-export([start_link/0, create_dialog/3, terminate_dialog/1, lookup_dialog/1, next_sequence/1, list_dialogs/0, dialog_id/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {table}).

-type gen_from() :: {pid(), term()}.

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
%% @doc Creates dialog server as part of the supervision tree.
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


-spec create_dialog(uac | uas, #sip_request{}, #sip_response{}) -> ok.
%% @doc Create new dialog for given pair of request/response
%% @end
create_dialog(Kind, Request, Response) when
  is_record(Request, sip_request),
  is_record(Response, sip_response),
  (Kind =:= uac orelse Kind =:= uas) ->
    Dialog = dialog_state(Kind, Request, Response),
    gen_server:call(?SERVER, {create_dialog, Dialog}).

-spec terminate_dialog(#sip_dialog_id{}) -> ok.
%% @doc Terminate dialog based on dialog id
%% @end
terminate_dialog(DialogId) when is_record(DialogId, sip_dialog_id) ->
    gen_server:call(?SERVER, {terminate_dialog, DialogId}).

lookup_dialog(DialogId) ->
    gen_server:call(?SERVER, {lookup_dialog, DialogId}).

%% @doc Determine dialog id base on the message within dialog
%% @end
dialog_id(Kind, Msg) ->
    From = sip_message:header_top_value(from, Msg),
    CallId = sip_message:header_top_value('call-id', Msg),
    To = sip_message:header_top_value(to, Msg),

    ToTag = tag(to, To),
    FromTag = tag(from, From),
    case Kind of
        uac ->
            #sip_dialog_id{call_id = CallId, local_tag = FromTag, remote_tag = ToTag};
        uas ->
            #sip_dialog_id{call_id = CallId, local_tag = ToTag, remote_tag = FromTag}
    end.

-spec next_sequence(#sip_dialog_id{}) -> {ok, integer()} | {error, no_dialog}.
%% @doc Increase local sequence number by one and return new value
%% @end
next_sequence(#sip_dialog_id{} = DialogId) ->
    gen_server:call(?SERVER, {next_sequence, DialogId}).

-spec list_dialogs() -> [#sip_dialog{}].
%% @doc List all active dialogs
%% @end
list_dialogs() ->
    gen_server:call(?SERVER, list_dialogs).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    Table = ets:new(?MODULE, [private, set, {keypos, #sip_dialog.id}]),
    {ok, #state{table = Table}}.

%% @private
-spec handle_call({create_dialog, #sip_dialog{}}, gen_from(), #state{}) -> {reply, ok, #state{}};
                 (list_dialogs, gen_from(), #state{}) -> {reply, [#sip_dialog{}], #state{}}.
handle_call({create_dialog, Dialog}, _Client, State) ->
    case ets:insert_new(State#state.table, Dialog) of
        true -> {reply, ok, State};
        false -> {reply, {error, dialog_exists}, State}
    end;

handle_call({terminate_dialog, DialogId}, _Client, State) ->
    true = ets:delete(State#state.table, DialogId),
    {reply, ok, State};

handle_call({lookup_dialog, DialogId}, _Client, State) ->
    case ets:lookup(State#state.table, DialogId) of
        [] -> {reply, {error, no_dialog}, State};
        [Dialog] -> {reply, {ok, Dialog}, State}
    end;

handle_call({next_sequence, DialogId}, _Client, State) ->
    case catch ets:update_counter(State#state.table, DialogId, {#sip_dialog.local_seq, 1}) of
        {'EXIT', {badarg, _Pos}} ->
            {reply, {error, no_dialog}, State};
        Sequence ->
            {reply, {ok, Sequence}, State}
    end;

handle_call(list_dialogs, _Client, State) ->
    List = ets:tab2list(State#state.table),
    {reply, List, State};

handle_call(Request, _From, State) ->
    {stop, {unexpected, Request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {stop, {unexpected, term()}, #state{}}.
handle_cast(Cast, State) ->
    {stop, {unexpected, Cast}, State}.

%% @private
-spec handle_info(term(), #state{}) -> {stop, {unexpected, term()}, #state{}}.
handle_info(Info, State) ->
    {stop, {unexpected, Info}, State}.

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

-spec dialog_state(uac | uas, #sip_request{}, #sip_response{}) -> #sip_dialog{}.
%% @doc Construct dialog state for UAC/UAS
%% @end
dialog_state(Kind, Request, Response) ->
    Via = sip_message:header_top_value(via, Request),
    CSeq = sip_message:header_top_value(cseq, Request),
    From = sip_message:header_top_value(from, Request),
    CallId = sip_message:header_top_value('call-id', Request),
    % MUST have tag for uas, MAY have no tag for uac (RFC 2543 does not enforce usage of tags)
    To = sip_message:header_top_value(to, Response),

    ToTag = tag(to, To),
    FromTag = tag(from, From),

    IsSecure = (Via#sip_hdr_via.transport =:= tls) andalso sip_uri:is_sips(Request#sip_request.uri),

    case Kind of
        uas ->
            RemoteContact = sip_message:header_top_value(contact, Request),
            RouteSet = [Address#sip_hdr_address.uri || Address <- sip_message:header_values('record-route', Request)],
            DialogId = #sip_dialog_id{call_id = CallId, local_tag = ToTag, remote_tag = FromTag},

            #sip_dialog{id = DialogId,
                        % XXX: we generate new sequence eagerly, so we can use ets:update_counter to update it
                        local_seq = sip_idgen:generate_cseq(),
                        remote_seq = CSeq#sip_hdr_cseq.sequence,
                        local_uri = To#sip_hdr_address.uri,
                        remote_uri = From#sip_hdr_address.uri,
                        remote_target_uri = RemoteContact#sip_hdr_address.uri,
                        secure = IsSecure,
                        route_set = RouteSet};

        uac ->
            RemoteContact = sip_message:header_top_value(contact, Response),
            RouteSet = lists:reverse([Address#sip_hdr_address.uri || Address <- sip_message:header_values('record-route', Response)]),
            DialogId = #sip_dialog_id{call_id = CallId, local_tag = FromTag, remote_tag = ToTag},
            #sip_dialog{id = DialogId,
                        local_seq = CSeq#sip_hdr_cseq.sequence,
                        remote_seq = undefined,
                        local_uri = From#sip_hdr_address.uri,
                        remote_uri = To#sip_hdr_address.uri,
                        remote_target_uri = RemoteContact#sip_hdr_address.uri,
                        secure = IsSecure,
                        route_set = RouteSet}
    end.

tag(HeaderName, Header) ->
    case lists:keyfind(tag, 1, Header#sip_hdr_address.params) of
        false when HeaderName =:= from ->
            % UAC must be prepared to receive no tag in To:
            % UAS must be prepared to receive no tag in From:
            <<>>;
        {tag, Tag} ->
            Tag
    end.

