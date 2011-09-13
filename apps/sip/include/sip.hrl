%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP implementation data types.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% SIP core data types
%%-----------------------------------------------------------------

-type sip_name() :: atom() | binary().
%% Optimized representation of entity name. When name is constructed
%% from the binary, a check is made if matching atom exists. If such atom
%% exists, it is used to represent the name of the entity. Otherwise,
%% binary is used.
%%
%% This ensures both optimal representation of name at runtime without
%% the danger of overloading atom table by malicious requests.
%%
%% Note that client code can always safely match such name against atom,
%% since matching against atom ensures atom is loaded and therefore, atom
%% representation will be chosen by SIP libary. The opposite is not true,
%% though. Client code should never assume that name is binary, even if
%% client code itself does not contain an atom (because this atom could
%% be in fact referenced by another module).
-export_type([sip_name/0]).

-type sip_headers() :: [{Name :: sip_name(), Value :: any()}].
-type sip_params()  :: [{Name :: sip_name(), Value :: any()}].

%% Parsed SIP URI representation
%%
%% <em>Note that `user' can have escape sequences in it. It is not
%% unescaped, so the original URI can be always reconstructed from
%% the parts</em>.
-record(sip_uri, {scheme = sip      :: sip | sips,
                  user = <<>>       :: binary(),
                  password = <<>>   :: binary(),
                  host = undefined  :: inet:ip_address() | string(),
                  port = undefined  :: undefined | inet:ip_port(),
                  params = []       :: sip_params(),
                  headers = []      :: sip_headers()}).
-record(tel_uri, {phone = <<>> :: binary()}).
-type sip_uri() :: #sip_uri{} | #tel_uri{} | binary().


%% SIP message types.

-record(sip_request, {method        :: sip_name(),
                      uri           :: sip_uri(),
                      headers = []  :: sip_headers(),
                      body = <<>>   :: binary()}).
-record(sip_response, {status       :: integer(),
                       reason       :: binary(),
                       headers = [] :: sip_headers(),
                       body = <<>>  :: binary()}).

-type sip_message() :: #sip_request{} | #sip_response{}.

%% Headers

%% Value for headers `Accept:', ...
-record(sip_hdr_mediatype,
        {type :: sip_syntax:name(),
         subtype :: sip_syntax:name(),
         params = []}).

%% Value for headers `Accept-Encoding:', ...
-record(sip_hdr_encoding,
        {encoding :: sip_syntax:name(),
         params = []}).

%% Value for headers `Accept-Language:', ...
-record(sip_hdr_language,
        {language :: sip_syntax:name(),
         params = []}).

%% Value for headers `Alert-Info:', `Call-Info', `Error-Info', ...
-record(sip_hdr_info,
        {uri :: #sip_uri{} | binary(),
         params = []}).

%% Value for header `Authorization:'
-record(sip_hdr_auth,
        {scheme :: sip_syntax:name(),
         params = []}).

%% Value for header `Content-Disposition:'
-record(sip_hdr_disposition,
        {type :: sip_syntax:name(),
         params = []}).

%% Value for header `Retry-After:'
-record(sip_hdr_retry,
        {delay :: integer(),
         comment = <<>> :: binary(),
         params = []}).

%% Value for header `Timestamp:'
-record(sip_hdr_timestamp,
        {timestamp :: float(),
         delay = 0.0 :: float()}).

%% Value for header `Warning:'
-record(sip_hdr_warning,
        {code :: integer(),
         agent :: binary(),
         text :: binary()}).

-record(sip_hdr_via, {version = <<"2.0">> :: binary(),
                      % note that tcp+tls becomes 'tls' transport
                      transport :: 'udp' | 'tcp' | 'tls' | atom(),
                      host :: inet:ip_address() | string(), % sent-by hostname
                      port = 'undefined' :: integer() | 'undefined', % sent-by port
                      params = [] :: [{'ttl', integer()} |
                                      {'maddr', string() | inet:ip_address()} |
                                      {'received', inet:ip_address()} |
                                      {'branch', binary()} |
                                      {sip_syntax:name(), term()} | sip_syntax:name()]}).

-record(sip_hdr_cseq, {sequence :: integer(), method :: atom() | binary()}).

%% Value for address headers (`Route:', `Record-Route', `To:', `From:', `Contact')
-record(sip_hdr_address,
        {display_name = <<>> :: binary(), % display name is unquoted (all escapes are unescaped)
         uri = <<>>          :: binary() | #sip_uri{},
         params = []         :: [{sip_syntax:name(), term()} | sip_syntax:name()]}).

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
-record(sip_tx_client, {branch :: binary(), method :: sip_syntax:name()}).

%% Server transaction unique key
-record(sip_tx_server, {host :: binary(), % via sent-by host
                        port :: integer() | 'undefined', % via sent-by port,
                        branch :: binary(),
                        method :: sip_syntax:name()}).

%%-----------------------------------------------------------------
%% SIP dialogs
%%-----------------------------------------------------------------

%% Dialog is identified by call-id, local and remote tags.
-record(sip_dialog, {local_tag, remote_tag, call_id}).

%%-----------------------------------------------------------------
%% SIP UAC/UAS
%%-----------------------------------------------------------------

-define(CORE_PROPERTY, core_registration).
-record(sip_core_info, {is_applicable :: fun((sip_message()) -> boolean())}). % check if message is applicable
