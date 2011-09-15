%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog API
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_dialog).

%% API
-export([create_dialog/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

-define(SERVER, ?MODULE).

%% API

-spec create_dialog(uac | uas, #sip_request{}, #sip_response{}) -> ok.
%% @doc Create new dialog
%% @end
create_dialog(uas, Request, Response) ->
    Dialog = uas_dialog_state(Request, Response),
    gen_server:call(?SERVER, {create_dialog, Dialog}).

%% Internal functions

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
    IsSecureURI =
        case Request#sip_request.uri of
            #sip_uri{scheme = sips} -> true;
            _Other -> false
        end,

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

