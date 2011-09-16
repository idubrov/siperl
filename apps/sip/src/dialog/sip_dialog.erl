%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog server
%%%
%%% Manages SIP dialog creation, state, updates and expiration.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_dialog).

%% API
-export([start_link/0, create_dialog/3, is_dialog_establishing/2]).

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
create_dialog(uas, Request, Response) ->
    Dialog = uas_dialog_state(Request, Response),
    gen_server:call(?SERVER, {create_dialog, Dialog}).

-spec is_dialog_establishing(#sip_request{}, #sip_response{}) -> boolean().
%% @doc Check if response to the request will establish a new dialog
%%
%% Response is considered as dialog establishing if it is response
%% to the `INVITE' method, status code is 2xx and no tag is present
%% in the request `To' header.
%% @end
is_dialog_establishing(#sip_request{method = 'INVITE'} = Request, #sip_response{status = Status})
  when Status >= 200, Status =< 299 ->
    To = sip_message:header_top_value(to, Request),
    case lists:keyfind(tag, 1, To#sip_hdr_address.params) of
        false -> true;
        {tag, _ToTag} -> false
    end;
is_dialog_establishing(#sip_request{}, #sip_response{}) -> false.

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------

%% @private
-spec init({}) -> {ok, #state{}}.
init({}) ->
    Table = ets:new(?MODULE, [private, set, {keypos, #sip_dialog.id}]),
    {ok, #state{table = Table}}.

%% @private
-spec handle_call({create_dialog, #sip_dialog{}}, gen_from(), #state{}) -> {reply, ok, #state{}}.
handle_call({create_dialog, Dialog}, _Client, State) ->
    case ets:insert_new(State#state.table, Dialog) of
        true -> {reply, ok, State};
        false -> {reply, {error, already_exist}, State}
    end;
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

-spec uas_dialog_state(#sip_request{}, #sip_response{}) -> #sip_dialog{}.
%% @doc Construct dialog state for UAS
%% @end
uas_dialog_state(Request, Response)
  when is_record(Request, sip_request), is_record(Response, sip_response) ->
    Via = sip_message:header_top_value(via, Request),
    Contact = sip_message:header_top_value(contact, Request),
    CSeq = sip_message:header_top_value(cseq, Request),
    From = sip_message:header_top_value(from, Request),
    CallId = sip_message:header_top_value('call-id', Request),
    % To comes from the response (MUST have tag)
    To = sip_message:header_top_value(to, Response),

    ToTag = tag(to, To),
    FromTag = tag(from, From),

    IsViaTLS = Via#sip_hdr_via.transport =:= tls,
    IsSecureURI = sip_uri:is_sips(Request#sip_request.uri),

    RouteSet = [Address#sip_hdr_address.uri || Address <- sip_message:header_values('record-route', Request)],
    DialogId = #sip_dialog_id{call_id = CallId, local_tag = ToTag, remote_tag = FromTag},

    #sip_dialog{id = DialogId,
                local_seq = undefined,
                remote_seq = CSeq#sip_hdr_cseq.sequence,
                local_uri = To#sip_hdr_address.uri,
                remote_uri = From#sip_hdr_address.uri,
                remote_target_uri = Contact#sip_hdr_address.uri,
                secure = (IsViaTLS andalso IsSecureURI),
                route_set = RouteSet}.


tag(HeaderName, Header) ->
    case lists:keyfind(tag, 1, Header#sip_hdr_address.params) of
        false when HeaderName =:= from ->
            <<>>; % UAS must be prepared to receive no tag in From:
        {tag, Tag} ->
            Tag
    end.

