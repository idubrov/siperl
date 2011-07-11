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
-record(sip_message, {start_line :: sip_message:start_line(), headers = [] :: [sip_headers:header()], body = <<"">> :: binary()}).

-record(sip_hdr_via, {version = <<"2.0">> :: binary(), 
					  transport :: atom(), 
					  sent_by = {<<"_HOST_">>, 0} :: sip_headers:via_sent_by(), 
					  params = [] :: [{binary() | atom(), term()} | binary() | atom()]}).

-record(sip_hdr_cseq, {sequence :: integer(), method :: sip_headers:method()}).
-record(sip_hdr_address, {display_name = <<>> :: binary(), 
                         uri = <<>> :: binary(), 
                         params = [] :: [{binary() | atom(), term()} | binary() | atom()]}).

%%-----------------------------------------------------------------
%% SIP transport layer types
%%-----------------------------------------------------------------

%% @doc
%% According to the RFC 3261 18, connections are indexed by the tuple
%% formed from the address, port, and transport protocol at the far end
%% of the connection
%% @end
-record(sip_destination, {address :: inet:ip_address() | inet:hostname(),
                          port = 5060 :: integer(),
                          transport :: atom()}).