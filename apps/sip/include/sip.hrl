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


-type sip_sequence() :: 0..2147483647. % sequence number MUST be less than 2**31

-type sip_headers() :: [{Name :: sip_name(), Value :: any()}].
-type sip_params()  :: [{Name :: sip_name(), Value :: any()} | sip_name()].

%% Parsed SIP URI representation
%%
%% <em>Note that `user' can have escape sequences in it. It is not
%% unescaped, so the original URI can be always reconstructed from
%% the parts</em>.
-record(sip_uri, {scheme = sip      :: sip | sips,
                  user = <<>>       :: binary(),
                  password = <<>>   :: binary(),
                  host = undefined  :: inet:ip_address() | string(),
                  port = undefined  :: undefined | 0..65535,
                  params = []       :: sip_params(),
                  headers = []      :: sip_headers()}).
-record(tel_uri, {phone = <<>> :: binary()}).
-type sip_uri() :: #sip_uri{} | #tel_uri{} | binary().


%% SIP message types.

-type sip_status()  :: 100..699.
%% 1xx Provisional
%% 2xx Success
%% 3xx Redirection
%% 4xx Client Error
%% 5xx Server Error
%% 6xx Global Failure

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
        {type :: sip_name(),
         subtype :: sip_name(),
         params = []}).

%% Value for headers `Accept-Encoding:', ...
-record(sip_hdr_encoding,
        {encoding :: sip_name(),
         params = []}).

%% Value for headers `Accept-Language:', ...
-record(sip_hdr_language,
        {language :: sip_name(),
         params = []}).

%% Value for headers `Alert-Info:', `Call-Info', `Error-Info', ...
-record(sip_hdr_info,
        {uri :: #sip_uri{} | binary(),
         params = []}).

%% Value for header `Authorization:'
-record(sip_hdr_auth,
        {scheme :: sip_name(),
         params = []}).

%% Value for header `Content-Disposition:'
-record(sip_hdr_disposition,
        {type :: sip_name(),
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
                                      {sip_name(), term()} | sip_name()]}).

-record(sip_hdr_cseq, {sequence :: sip_sequence(),
                       method :: atom() | binary()}).

%% Value for address headers (`Route:', `Record-Route', `To:', `From:', `Contact')
-record(sip_hdr_address,
        {display_name = <<>> :: binary(), % display name is unquoted (all escapes are unescaped)
         uri = <<>>          :: binary() | #sip_uri{},
         params = []         :: [{sip_name(), term()} | sip_name()]}).

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
-record(sip_tx_client, {branch :: binary(), method :: sip_name()}).

%% Server transaction unique key
-record(sip_tx_server, {host :: binary(), % via sent-by host
                        port :: integer() | 'undefined', % via sent-by port,
                        branch :: binary(),
                        method :: sip_name()}).

-type sip_tx_key() :: #sip_tx_client{} | #sip_tx_server{}.

%%-----------------------------------------------------------------
%% SIP dialogs
%%-----------------------------------------------------------------

-record(sip_dialog_id,
        {call_id :: binary(),
         local_tag :: binary(),
         remote_tag :: binary()}).

%% Dialog is identified by call-id, local and remote tags.

-record(sip_dialog,
        {id                 :: #sip_dialog_id{},
         local_seq          :: sip_sequence(),
         remote_seq         :: sip_sequence(),
         local_uri          :: sip_uri(),
         remote_uri         :: sip_uri(),
         remote_target_uri  :: sip_uri(),
         secure             :: boolean(),
         route_set          :: [sip_uri()]}).

%%-----------------------------------------------------------------
%% SIP UAC/UAS
%%-----------------------------------------------------------------

-define(CORE_PROPERTY, core_registration).
-record(sip_core_info, {is_applicable :: fun((sip_message()) -> boolean())}). % check if message is applicable
