%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog server
%%%
%%% Manages SIP dialog usage (creation, state, updates and expiration).
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_dialog).

%% API
%-export([create_dialog/3, destroy_dialog/1]).
-export([create_invite_usage/3, destroy_invite_usage/1]). % INVITE dialog usage
-export([update_session/2, update_session/3]). % INVITE dialog usage
-export([create_request/2, update_remote_seq/2, dialog_id/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% API

%% @doc Create INVITE usage of the dialog
%% @end
-spec create_invite_usage(uac | uas, #sip_request{}, #sip_response{}) -> {ok, #sip_dialog_id{}} | {error, dialog_exists}.
create_invite_usage(Kind, Request, Response) ->
    create_dialog(Kind, Request, Response).

%% @doc Destroy INVITE usage of the dialog
%% @end
-spec destroy_invite_usage(#sip_dialog_id{}) -> ok | {error, no_dialog}.
destroy_invite_usage(DialogId) when is_record(DialogId, sip_dialog_id) ->
    destroy_dialog(DialogId).


-spec dialog_id(uac | uas, sip_message()) -> #sip_dialog_id{}.
%% @doc Determine dialog id based on the message within dialog
%% Kind identifies the UA role related to the message received.
%% @end
dialog_id(Kind, Msg) ->
    From = sip_message:header_top_value(from, Msg),
    CallId = sip_message:header_top_value('call-id', Msg),
    To = sip_message:header_top_value(to, Msg),

    ToTag = tag(To),
    FromTag = tag(From),
    case Kind of
        uac ->
            #sip_dialog_id{call_id = CallId, local_tag = FromTag, remote_tag = ToTag};
        uas ->
            #sip_dialog_id{call_id = CallId, local_tag = ToTag, remote_tag = FromTag}
    end.

-spec create_request(sip_method(), #sip_dialog_id{}) -> {ok, #sip_request{}} | {error, no_dialog}.
%% @doc Create request within the dialog
%% @end
create_request('ACK', #sip_dialog_id{} = DialogId) when is_record(DialogId, sip_dialog_id)->
    case sip_dialog_ets:lookup_dialog(DialogId) of
        {error, no_dialog} ->
            {error, no_dialog};
        {ok, Dialog} ->
            {ok, create_request_internal('ACK', 0, Dialog)}
    end;
create_request(Method, #sip_dialog_id{} = DialogId) when is_record(DialogId, sip_dialog_id)->
    case sip_dialog_ets:next_local_seq(DialogId) of
        {error, no_dialog} ->
            {error, no_dialog};
        {ok, Dialog} ->
            Sequence = Dialog#sip_dialog.local_seq,
            {ok, create_request_internal(Method, Sequence, Dialog)}
    end.

%% @doc Update the remote sequence in the dialog state
%% @end
-spec update_remote_seq(#sip_dialog_id{}, sip_sequence()) -> ok | {error, no_dialog} | {error, out_of_order}.
update_remote_seq(#sip_dialog_id{} = DialogId, Sequence) ->
    sip_dialog_ets:update_remote_seq(DialogId, Sequence).

%%-----------------------------------------------------------------
%% INVITE dialog usage
%%-----------------------------------------------------------------

%% @doc Update session based on the offer/answer from the remote side
%% @end
-spec update_session(uac | uas, #sip_request{}) -> error_m:monad(ok).
update_session(UA, Request) when is_record(Request, sip_request) ->
    Result = sip_offer_answer:validate_request(Request),
    update_session_internal(UA, Result, Request).

%% @doc Update session based on the offer/answer from the local side
%% @end
-spec update_session(uac | uas, #sip_request{}, #sip_response{}) -> error_m:monad(ok).
update_session(UA, Request, Response) when is_record(Request, sip_request), is_record(Response, sip_response) ->
    Result = sip_offer_answer:validate_response(Request, Response),
    io:format("RESPONSE ~p ~p~n", [Request, Response]),
    update_session_internal(UA, Result, Response).

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

    ToTag = tag(To),
    FromTag = tag(From),

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
                        route_set = RouteSet,
                        owner = self()};

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
                        route_set = RouteSet,
                        owner = self()}
    end.

%% Construct the request within dialog
create_request_internal(Method, Sequence, Dialog) ->
    DialogId = Dialog#sip_dialog.id,

    To = {to,
          sip_headers:address(<<>>,
                              Dialog#sip_dialog.remote_uri,
                              tag_params(DialogId#sip_dialog_id.remote_tag))},
    From = {from,
            sip_headers:address(<<>>,
                                Dialog#sip_dialog.local_uri,
                                tag_params(DialogId#sip_dialog_id.local_tag))},
    CallId = {'call-id', DialogId#sip_dialog_id.call_id},
    CSeq = {cseq, sip_headers:cseq(Sequence, Method)},
    Via = {via, #sip_hdr_via{}},
    MaxForwards = {'max-forwards', 70},

    % Build the route set
    {RequestURI, Routes} = determine_routing(Dialog#sip_dialog.remote_target_uri,
                                             Dialog#sip_dialog.route_set),

    #sip_request{method = Method,
                 uri = RequestURI,
                 headers = [Via, MaxForwards, From, To, CSeq, CallId | Routes]}.

tag(Header) ->
    case lists:keyfind(tag, 1, Header#sip_hdr_address.params) of
        false ->
            % UAC must be prepared to receive no tag in To:
            % UAS must be prepared to receive no tag in From:
            <<>>;
        {tag, Tag} ->
            Tag
    end.

determine_routing(RemoteTargetURI, []) ->
    % 1. empty target route
    {RemoteTargetURI, []};
determine_routing(RemoteTargetURI, [TopRoute | RestRouteSet] = RouteSet) ->
    case sip_uri:is_loose_router(TopRoute) of
        true ->
            % 2. top route is loose router
            Routes = [{route, sip_headers:address(Route)} || Route <- RouteSet],
            {RemoteTargetURI, Routes};
        false ->
            % 3. top route is strict router
            RequestURI = strip_parameters(TopRoute),
            Routes = [{route, sip_headers:address(Route)} || Route <- RestRouteSet] ++ [RemoteTargetURI],
            {RequestURI, Routes}
    end.

%% @doc Convert null tag into empty list of parameters
%% @end
tag_params(<<>>) -> [];
tag_params(Tag) -> [{tag, Tag}].

%% @doc Strip any parameters that are not allowed in a Request-URI.
%% XXX: For now, all parameters are stripped.
%% @end
strip_parameters(URI) ->
    URI#sip_uri{params = []}.

-spec create_dialog(uac | uas, #sip_request{}, #sip_response{}) -> {ok, #sip_dialog_id{}} | {error, dialog_exists}.
%% @doc Create new dialog for given pair of request/response
%% @end
create_dialog(Kind, Request, Response) when
  is_record(Request, sip_request),
  is_record(Response, sip_response),
  (Kind =:= uac orelse Kind =:= uas) ->
    Dialog = dialog_state(Kind, Request, Response),
    case sip_dialog_ets:create_dialog(Dialog) of
        ok ->
            % notify about dialog creation
            gen_event:notify(sip_dialog_man, {dialog_created,
                                              Dialog#sip_dialog.id,
                                              Dialog#sip_dialog.owner}),
            {ok, Dialog#sip_dialog.id};
        Error -> Error
    end.

-spec destroy_dialog(#sip_dialog_id{}) -> ok | {error, no_dialog}.
%% @doc Terminate dialog based on dialog id
%% @end
destroy_dialog(DialogId) when is_record(DialogId, sip_dialog_id) ->
    case sip_dialog_ets:destroy_dialog(DialogId) of
        {ok, Owner} ->
            % notify about dialog termination
            gen_event:notify(sip_dialog_man, {dialog_terminated,
                                              DialogId,
                                              Owner});
        Error -> Error
    end.

%% @doc Update session based on the offer/answer that could be present in message
%% @end
update_session_internal(UA, Result, Msg) ->
    case Result of
        ok -> error_m:return(ok); % no offer/answer, nothing to update
        cancel ->
            io:format("FIXME: ~p SESSION CANCEL~n", [UA]),
            error_m:return(ok);
        Kind when Kind =:= offer; Kind =:= answer -> % offer or answer expected
            case sip_message:session(Msg) of
                false ->
                    error_m:fail(session_expected);
                #sip_session_desc{} = SessionDesc ->
                    io:format("FIXME: ~p SESSION ~p ~p~n", [UA, Kind, SessionDesc]),
                    error_m:return(ok)
            end
    end.
