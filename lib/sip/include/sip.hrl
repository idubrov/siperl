%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP implementation data types.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% SIP core messages
%%-----------------------------------------------------------------

%% @doc
%% SIP message types.
%% FIXME: note about header values (binary -- non-parsed, list -- multi-line header, etc)
%% @see RFC 3261 Chapter 7
%% @end
-record(sip_request, {method :: binary() | atom(), uri :: binary()}).
-record(sip_response, {status :: integer() | atom(), reason :: binary()}).
-record(sip_message, {kind :: #sip_request{} | #sip_response{},
                      headers = [] :: [{Name :: atom() | binary(), Value :: binary() | term()}],
                      body = <<"">> :: binary()}).

-record(sip_hdr_via, {version = <<"2.0">> :: binary(),
					  transport :: atom(),
                      host = <<>> :: binary(), % sent-by hostname
                      port = 'undefined' :: integer() | 'undefined', % sent-by port
					  params = [] :: [{binary() | atom(), term()} | binary() | atom()]}).

-record(sip_hdr_cseq, {sequence :: integer(), method :: atom() | binary()}).
-record(sip_hdr_address, {display_name = <<>> :: binary(),
                         uri = <<>> :: binary(),
                         params = [] :: [{binary() | atom(), term()} | binary() | atom()]}).
% FIXME: default uri should be "Anonymous" <sip:thisis@anonymous.invalid>

%%-----------------------------------------------------------------
%% SIP transport layer types
%%-----------------------------------------------------------------

%% @doc Destination to send message to
%% @end
-record(sip_destination, {address :: inet:ip_address(),
                          port = 5060 :: integer() | 'undefined',
                          transport :: atom()}).

%% @doc Transport layer connection
%% @end
-record(sip_connection, {transport :: atom(), connection :: pid() | term()}).

%%-----------------------------------------------------------------
%% SIP transaction layer
%%-----------------------------------------------------------------

%% @doc See RFC 3261, 20.42 Via
%% @end
-define(MAGIC_COOKIE, "z9hG4bK").

%% @doc Client transaction unique key
%% @end
-record(sip_tx_client, {branch :: binary(), method :: atom() | binary()}).

%% @doc Server transaction unique key
%% @end
-record(sip_tx_server, {host :: binary(), % via sent-by host
                        port :: integer() | 'undefined', % via sent-by port,
                        branch :: binary(),
                        method :: atom() | binary()}).

%%-----------------------------------------------------------------
%% SIP dialogs
%%-----------------------------------------------------------------

%% @doc Dialog is identified by call-id, local and remote tags.
%% @end
-record(sip_dialog, {local_tag, remote_tag, call_id}).