%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP implementation data types.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% SIP core messages
%%-----------------------------------------------------------------


%% Parsed SIP URI representation
%%
%% <em>Note that `user' can have escape sequences in it. It is not
%% unescaped, so the original URI can be always reconstructed from
%% the parts</em>.
-record(sip_uri, {scheme = sip      :: 'sip' | 'sips',
                  user = <<>>       :: binary(),
                  password = <<>>   :: binary(),
                  host = <<>>       :: inet:ip_address() | string(),
                  port = undefined  :: integer() | 'undefined',
                  params = [],
                  headers = []}).
-record(tel_uri, {phone = <<>> :: binary()}).

%% SIP message types.
%% FIXME: note about header values (binary -- non-parsed, list -- multi-line header, etc)
-record(sip_request, {method :: binary() | atom(), uri :: #sip_uri{} | #tel_uri{} | term()}).
-record(sip_response, {status :: integer() | atom(), reason :: binary()}).
-record(sip_message, {kind :: #sip_request{} | #sip_response{},
                      headers = [] :: [{Name :: atom() | binary(), Value :: binary() | term()}],
                      body = <<"">> :: binary()}).

%% Headers

%% Value for headers `Accept:', ...
-record(sip_hdr_mediatype,
        {type :: atom() | binary(),
         subtype :: atom() | binary(),
         params = []}).

%% Value for headers `Accept-Encoding:', ...
-record(sip_hdr_encoding,
        {encoding :: atom() | binary(),
         params = []}).

%% Value for headers `Accept-Language:', ...
-record(sip_hdr_language,
        {language :: atom() | binary(),
         params = []}).

%% Value for headers `Alert-Info:', `Call-Info'
-record(sip_hdr_info,
        {uri :: #sip_uri{} | binary(),
         params = []}).

%% Value for header `Authorization:'
-record(sip_hdr_auth,
        {scheme :: binary() | atom(),
         params = []}).

%% Value for header `Content-Disposition:'
-record(sip_hdr_disposition,
        {type :: binary() | atom(),
         params = []}).

-record(sip_hdr_via, {version = <<"2.0">> :: binary(),
                      % note that tcp+tls becomes 'tls' transport
                      transport :: 'udp' | 'tcp' | 'tls' | atom(),
                      host :: inet:ip_address() | string(), % sent-by hostname
                      port = 'undefined' :: integer() | 'undefined', % sent-by port
                      params = [] :: [{'ttl', integer()} |
                                      {'maddr', string() | inet:ip_address()} |
                                      {'received', inet:ip_address()} |
                                      {'branch', binary()} |
                                      {binary() | atom(), term()} |
                                      binary() | atom()]}).

-record(sip_hdr_cseq, {sequence :: integer(), method :: atom() | binary()}).

%% Value for address headers (`Route:', `Record-Route', `To:', `From:', `Contact')
-record(sip_hdr_address,
        {display_name = <<>> :: binary(), % display name is unquoted (all escapes are unescaped)
         uri = <<>>          :: binary() | #sip_uri{},
         params = []         :: [{binary() | atom(), term()} | binary() | atom()]}).

%%-----------------------------------------------------------------
%% SIP transport layer types
%%-----------------------------------------------------------------

%% Destination to send message to
-record(sip_destination, {address :: inet:ip_address(),
                          port = 5060 :: integer() | 'undefined',
                          % note that there is no 'tls' transport, it should be in params
                          transport :: 'udp' | 'tcp' | atom(),
                          params = [] :: ['tls' | {'ttl', integer()}]}).

%% Transport layer connection
-record(sip_connection, {transport :: atom(), connection :: pid() | term()}).

%%-----------------------------------------------------------------
%% SIP transaction layer
%%-----------------------------------------------------------------

%% See RFC 3261, 20.42 Via
-define(MAGIC_COOKIE, "z9hG4bK").

%% Client transaction unique key
-record(sip_tx_client, {branch :: binary(), method :: atom() | binary()}).

%% Server transaction unique key
-record(sip_tx_server, {host :: binary(), % via sent-by host
                        port :: integer() | 'undefined', % via sent-by port,
                        branch :: binary(),
                        method :: atom() | binary()}).

%%-----------------------------------------------------------------
%% SIP dialogs
%%-----------------------------------------------------------------

%% Dialog is identified by call-id, local and remote tags.
-record(sip_dialog, {local_tag, remote_tag, call_id}).

%%-----------------------------------------------------------------
%% SIP UAC/UAS
%%-----------------------------------------------------------------


-record(req_info, {request :: #sip_message{},                 % SIP request message
                   destinations = [] :: [#sip_destination{}], % list of IP addresses to try next
                   target_set = sip_priority_set:new(),       % URI to visit next (redirects)
                   user_data}).                               % Custom user data associated with request

-record(sip_ua_state, {requests = dict:new(),       % Requests being sent by UAC
                       callback :: module(),        % Callback module
                       allow = [] :: [atom()],      % List of allowed methods
                       supported = [] :: [atom()],  % List of supported extensions
                       state}).                     % Callback module state

-define(CORE_PROPERTY, core_registration).
-record(sip_core_info, {is_applicable :: fun((#sip_message{}) -> boolean())}). % check if message is applicable

