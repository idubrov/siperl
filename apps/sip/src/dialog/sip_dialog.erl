%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP dialog API
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_dialog).

%% API
-export([from_message/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% API

-spec from_message(uac | uas, sip_message()) -> sip_dialog_id().
%% @doc Determine dialog id from the message
%% @end
from_message(Kind, Msg) ->
    CallId = sip_message:header_top_value('call-id', Msg),
    ToTag = tag(to, Msg),
    FromTag = tag(from, Msg),
    case Kind of
        uac ->
            {CallId, FromTag, ToTag};
        uas ->
            {CallId, ToTag, FromTag}
    end.

tag(HeaderName, Msg) ->
    Header = sip_message:header_top_value(HeaderName, Msg),
    case lists:keyfind(tag, 1, Header#sip_hdr_address.params) of
        false ->
            undefined;
        {tag, Tag} ->
            Tag
    end.

