%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP messages parsing/generation.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_message).
-compile({parse_transform, do}).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
-export([method/1, is_secure/1, is_within_dialog/1, is_dialog_establishing/1]).
-export([validate_request/1, validate_response/1, validate_contact/2]).
-export([parse_stream/2, parse_datagram/1, parse_all_headers/1, to_binary/1]).
-export([create_ack/2, create_cancel/1, create_response/2, create_response/3]).
-export([update_top_header/3, replace_top_header/3, append_header/3]).
-export([header_values/2, header_top_value/2, has_header/2]).
-export([with_branch/2, foldl_headers/4]).
-export([default_reason/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("../sip_common.hrl").
-include("sip_syntax.hrl").
-include("sip.hrl").

%% Internal state
%% 'BEFORE' -- state before Start-Line
%% 'HEADERS' -- state after first Start-Line character was received
%% {'BODY', Message, Length} -- state after receiving headers, but before body (\r\n\r\n)
-type state() :: {'BEFORE' | 'HEADERS' | {'BODY', sip_message(), integer()}, binary()}.

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec is_secure(#sip_request{}) -> boolean().
%% @doc Check if SIP request requires TLS
%% @end
is_secure(Request) when is_record(Request, sip_request) ->
    % either Record-Route or Contact, if Record-Route is not present, 12.1.1
    #sip_hdr_address{uri = RROrContactURI} =
        case sip_message:has_header('record-route', Request) of
            true -> sip_message:header_top_value('record-route', Request);
            false -> sip_message:header_top_value(contact, Request)
        end,

    sip_uri:is_sips(Request#sip_request.uri) orelse sip_uri:is_sips(RROrContactURI).

-spec is_within_dialog(#sip_request{}) -> boolean().
%% @doc Check if SIP request is within dialog (has tag in To: header)
%% @end
is_within_dialog(#sip_request{} = Request) ->
    To = sip_message:header_top_value(to, Request),
    case lists:keyfind(tag, 1, To#sip_hdr_address.params) of
        false -> false;
        {tag, _ToTag} -> true
    end.

-spec is_dialog_establishing(#sip_request{}) -> boolean().
%% @doc Check if SIP request is dialog-establishing
%% Dialog-establishing request is 'INVITE' which is outside of the dialog.
%% @end
is_dialog_establishing(#sip_request{method = 'INVITE'} = Request) ->
    not is_within_dialog(Request);
is_dialog_establishing(#sip_request{}) ->
    false.

%% @doc Retrieve method of SIP message
%%
%% Returns `Method' from `start-line' for requests, `Method' from `CSeq' header
%% for responses.
%% @end
-spec method(sip_message()) -> sip_name().
method(#sip_request{method = Method}) -> Method;
method(#sip_response{} = Msg) ->
    CSeq = header_top_value(cseq, Msg),
    CSeq#sip_hdr_cseq.method.

-spec to_binary(sip_message()) -> binary().
to_binary(Msg) ->
    Top = case Msg of
              #sip_request{method = Method, uri = URI} ->
                  URIBin = sip_uri:format(URI),
                  MethodBin = sip_syntax:format_name(Method),
                  <<MethodBin/binary, " ", URIBin/binary, " ", ?SIPVERSION>>;
              #sip_response{status = Status, reason = Reason} ->
                  StatusStr = sip_binary:integer_to_binary(Status),
                  <<?SIPVERSION, " ", StatusStr/binary, " ", Reason/binary>>
          end,
    Headers = sip_headers:format_headers(headers(Msg)),
    iolist_to_binary([Top, <<"\r\n">>, Headers, <<"\r\n">>, body(Msg)]).


%% @doc Update value of top header with given name.
%%
%% If header value is multi-value, only first element of the list (top header)
%% is updated by the function.
%%
%% <em>Note: this function parses the header value if header is in binary form.</em>
%% <em>If no header is found with given name, update function is called with
%% `undefined' parameter. If function returns any value other than `undefined',
%% a new header with that value is added.</em>
%% @end
-spec update_top_header(
        sip_name(),
        fun((Value :: term()) -> UpdatedValue :: term()),
        sip_message()) -> sip_message().
update_top_header(HeaderName, Fun, Msg) ->
    Headers = update_header(HeaderName, Fun, headers(Msg)),
    set_headers(Msg, Headers).

%% Internal function to update the header list
update_header(HeaderName, Fun, [{HeaderName, Value} | Rest]) ->
    % Parse header if it is not parsed yet
    UpdatedValue =
        case sip_headers:parse(HeaderName, Value) of
            % multi-value header
            [Top | Rest2] -> [Fun(Top) | Rest2];
            % single value header
            Top -> Fun(Top)
        end,
    [{HeaderName, UpdatedValue} | Rest];
update_header(HeaderName, Fun, [Header | Rest]) ->
    [Header | update_header(HeaderName, Fun, Rest)];
update_header(HeaderName, Fun, []) ->
    % generate a new header value
    case Fun(undefined) of
        undefined -> [];
        Value -> [{HeaderName, Value}]
    end.

%% @doc Replace value of the top header with given name
%%
%% Replace value of top header with given name with provided value. If
%% header value is multi-value (a list), the first element of the list
%% is replaced.
%%
%% <em>Note that header is added automatically, if there is no header with given name</em>
%% @end
-spec replace_top_header(sip_name(), term() | binary(), sip_message()) -> sip_message().
replace_top_header(HeaderName, Value, Message) ->
    update_top_header(HeaderName, fun (_Old) -> Value end, Message).

%% @doc Append header with given name and value
%%
%% Appends header with given name and value to the end of the headers list.
%% <em>If header value is empty list, message is not modified</em>
%% @end
-spec append_header(sip_name(), term() | binary(), sip_message()) -> sip_message().
append_header(_HName, [], Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    Msg;
append_header(HName, Value, Msg) ->
    set_headers(Msg, headers(Msg) ++ [{HName, Value}]).

%% @doc Update top `Via:' header of given request with provided branch value
%% @end
-spec with_branch(binary(), sip_message()) -> sip_message().
with_branch(Branch, #sip_request{} = Msg) when is_binary(Branch) ->
    UpdateBranch =
        fun (Via) ->
                 Params = lists:keystore(branch, 1, Via#sip_hdr_via.params, {branch, Branch}),
                 Via#sip_hdr_via{params = Params}
        end,
    update_top_header(via, UpdateBranch, Msg).

%% @doc Calls `Fun(Value, AccIn)' on all successive header values named `Name'
%%
%% <em>Note: this function parses the header value if header is in binary form.</em>
%% @end
-spec foldl_headers(sip_name(),
                    fun ((Value::term(), AccIn::term()) -> AccOut :: term()),
                    term(),
                    sip_message()) -> Acc :: term().
foldl_headers(Name, Fun, Acc0, Msg) when
  is_function(Fun, 2),
  (is_record(Msg, sip_request) orelse is_record(Msg, sip_response)) ->
    Values = header_values(Name, Msg),
    lists:foldl(Fun, Acc0, Values).

%% @doc Retrieve values for given header name
%%
%% Retrieve value of given header. Accepts either full SIP message or list of
%% headers. Values of several headers with the same name are merged into single
%% list.
%% <em>Note: this function parses the header value if header is in binary form.</em>
%% <em>Note: this function is safe to use only with headers that allow merging
%% multiple values together. For example, `Authentication-Info' or `Contact' header
%% with value `*' do NOT allow this). See RFC 3261, Section 7.3</em>
%% @end
-spec header_values(sip_name(), sip_message() | sip_headers()) -> [any()].
header_values(Name, Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    header_values(Name, headers(Msg));
header_values(Name, Headers) when is_list(Headers) ->
    Filtered = lists:filter(fun ({N, _Value}) -> N =:= Name end, Headers),
    Parsed = lists:map(fun ({_Name, Value}) -> sip_headers:parse(Name, Value) end, Filtered),
    lists:flatten(Parsed).

%% @doc Retrieve top value for given header name
%%
%% Fails if header with given name does not exist.
%% <em>This function parses the header value if header is in binary form.</em>
%% @end
-spec header_top_value(sip_name(), sip_message() | sip_headers()) -> term().
header_top_value(Name, Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    Headers = headers(Msg),
    {Name, Value} = lists:keyfind(Name, 1, Headers),
    case sip_headers:parse(Name, Value) of
        [Top | _] -> Top; % support for multiple header values
        Top when not is_list(Top) -> Top
    end.

%% @doc Check if SIP message has header
%% @end
-spec has_header(sip_name(), sip_message()) -> boolean().
has_header(Name, Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    Headers = headers(Msg),
    case lists:keyfind(Name, 1, Headers) of
        {Name, _Value} -> true;
        false -> false
    end.

%% @doc RFC 3261, 17.1.1.3 Construction of the ACK Request
%% @end
-spec create_ack(#sip_request{}, #sip_response{}) -> #sip_request{}.
create_ack(#sip_request{} = Request, #sip_response{} = Response) ->
    % To goes from the response (because of tag)
    To = header_top_value(to, Response),
    create_ack_cancel(Request, To, 'ACK').

%% @doc RFC 3261, 9.1 Constructing CANCEL requests
%% @end
-spec create_cancel(#sip_request{}) -> #sip_request{}.
create_cancel(#sip_request{} = Request) ->
    To = header_top_value(to, Request),
    create_ack_cancel(Request, To, 'CANCEL').


%% Internal method to create ACK/CANCEL requests (they are similar)
create_ack_cancel(Request, To, Method) ->
    % Call-Id, From, CSeq (only numberic part) and Route are taken from
    % the original request
    ACKHeaders =
        [case Name of
             'call-id' -> {Name, Value};
             from -> {Name, Value};
             cseq ->
                 CSeq = sip_headers:parse(cseq, Value),
                 CSeq2 = CSeq#sip_hdr_cseq{method = Method},
                 {Name, CSeq2};
             route ->
                 {Name, Value};
             % Also, Max-Forwards is copied as well. Although this is not explicitly
             % required by 17.1.1.3 or 9.1, it is required by 8.1.1
             'max-forwards' -> {Name, Value}
         end || {Name, Value} <- Request#sip_request.headers,
                (Name =:= 'call-id' orelse Name =:= from orelse
                 Name =:= cseq orelse Name =:= route orelse
                 Name =:= 'max-forwards')],

    % Via is taken from top Via of the original request
    Via = header_top_value(via, Request),

    #sip_request{method = Method,
                 uri = Request#sip_request.uri,
                 headers = [{'via', [Via]}, {'to', To} | ACKHeaders]}.


%% @doc Create response for given request
%% @end
-spec create_response(#sip_request{}, integer(), binary()) -> #sip_response{}.
create_response(#sip_request{headers = ReqHeaders}, Status, Reason) ->
    % FIXME: Copy Timestamp: for provisional responses
    Headers = [{Name, Value} || {Name, Value} <- ReqHeaders,
                                (Name =:= 'from' orelse Name =:= 'call-id' orelse
                                 Name =:= cseq orelse Name =:= 'via' orelse
                                 Name =:= 'to')],
    #sip_response{status = Status,
                  reason = Reason,
                  headers = Headers}.

%% @doc Create response for given request (with default Reason)
%% @end
-spec create_response(sip_message(), integer()) -> #sip_response{}.
create_response(Request, Status) ->
    create_response(Request, Status, default_reason(Status)).

%% @doc
%% Parses the datagram for SIP packet. The headers of the returned message are
%% retained in binary form for performance reasons. Use {@link parse_whole/1}
%% to parse the whole message or {@link sip_headers:parse/2} to parse
%% single header.
%% @end
-spec parse_datagram(Datagram :: binary()) ->
          {ok, sip_message()}
        | {error, content_too_small, sip_message()}.
parse_datagram(Datagram) ->
    {Pos, _Length} = binary:match(Datagram, <<"\r\n\r\n">>),
    Pos2 = Pos + 2,
    <<Top:Pos2/binary, "\r\n", Body/binary>> = Datagram,
    [Start, HeadersBin] = binary:split(Top, <<"\r\n">>),
    Headers = sip_headers:parse_headers(HeadersBin),

    Msg = parse_start_line(Start),
    Msg2 = set_headers(Msg, Headers),

    % RFC 3261 18.3
    case content_length(Headers) of
        % Content-Length is present
        {ok, ContentLength} when ContentLength =< size(Body) ->
            <<Body2:ContentLength/binary, _/binary>> = Body,
            {ok, set_body(Msg2, Body2)};
        {ok, _} ->
            {error, content_too_small, Msg2};
        % Content-Length is not present
        false ->
            {ok, set_body(Msg2, Body)}
    end.

%% @doc Parses the stream for complete SIP messages.

%% Return new parser state and possibly a message extracted from the
%% stream. The headers of the returned messages are retained in binary
%% form for performance reasons. Use {@link parse_whole/1} to parse the
%% whole message or {@link sip_headers:parse/2} to parse single header.
%%
%% <em>Note: caller is required to call this method with empty packet (<<>>)
%% until no new messages are returned</em>
%% @end
-spec parse_stream(Packet :: binary(), State :: state() | 'none') ->
          {ok, state()} |
          {ok, sip_message(), state()} |
          {error, no_content_length, sip_message(), state()}.
parse_stream(Packet, none) -> parse_stream(Packet, {'BEFORE', <<>>});
parse_stream(Packet, {State, Frame}) when is_binary(Packet) ->
    NewFrame = <<Frame/binary, Packet/binary>>,
    parse_stream_internal({State, NewFrame}, size(Frame)).

%% @doc Parses all headers of the message.
%% @end
-spec parse_all_headers(sip_message()) -> sip_message().
parse_all_headers(Msg) when is_record(Msg, sip_request); is_record(Msg, sip_response) ->
    Headers = [{Name, sip_headers:parse(Name, Value)} || {Name, Value} <- headers(Msg)],
    set_headers(Msg, Headers).


%% @doc Validate that request contains all required headers
%% A valid SIP request formulated by a UAC MUST, at a minimum, contain
%% the following header fields: To, From, CSeq, Call-ID, Max-Forwards,
%% and Via; all of these header fields are mandatory in all SIP
%% requests.
%% FIXME: UAC should validate generated request according to 12.1.2
%% @end
-spec validate_request(#sip_request{}) -> ok | {error, Reason :: term()}.
validate_request(#sip_request{method = Method} = Msg) ->
    do([error_m ||
        check(count(to, Msg) =:= 1, {invalid, to}),
        check(count(from, Msg) =:= 1, {invalid, from}),
        check(count(cseq, Msg) =:= 1, {invalid, cseq}),
        check(count('call-id', Msg) =:= 1, {invalid, 'call-id'}),
        % Although Max-Forwards is enforced by 8.1.1, it is not included by all
        % implementations. Moreover, proxy must pass requests without Max-Forwards
        % So, we do not enforce it here, see Issue #7.
        %check(count('max-forwards', Msg) =:= 1, {invalid, 'max-forwards'}),
        check(count(via, Msg) >= 1, {invalid, via}),
        check(Method =/= 'INVITE' orelse is_single_contact(Msg), {invalid, contact}), % 8.1.1.8
        check(is_contact_secure(Msg), {invalid, contact_must_be_sips})]).

-spec validate_response(#sip_response{}) -> ok | {error, Reason :: term()}.
%% @doc Validate that response contains all required headers
%% A valid SIP response formulated by a UAS MUST, at a minimum, contain
%% the following header fields: To, From, CSeq, Call-ID and Via;
%% all of these header fields are mandatory in all SIP responses.
%%
%% @end
validate_response(#sip_response{} = Response) ->
    do([error_m ||
        check(count(to, Response) =:= 1, {invalid, to}),
        check(count(from, Response) =:= 1, {invalid, from}),
        check(count(cseq, Response) =:= 1, {invalid, cseq}),
        check(count('call-id', Response) =:= 1, {invalid, 'call-id'}),
        check(count(via, Response) >= 1, {invalid, via})]).


-spec validate_contact(boolean(), sip_message()) -> ok | {error, Reason :: term()}.
%% @doc Validate the Contact header according to the 12.1.1
%% @end
validate_contact(Secure, Response) ->
    do([error_m ||
        % Must be exactly one Contact with SIP/SIPS uri
        ContactURI <-
            case header_values(contact, Response) of
                [#sip_hdr_address{uri = #sip_uri{} = C}] -> return(C);
                _Other -> fail({invalid, contact})
            end,
        % if Secure is true, Contact MUST be SIPS
        case sip_uri:is_sips(ContactURI) of
            false when Secure -> fail({invalid, contact_must_be_sips});
            _Other -> return(ok)
        end]).


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

parse_stream_internal({'BEFORE', <<"\r\n", Rest/binary>>}, _From) ->
    % RFC 3261 7.5  Implementations processing SIP messages over
    % stream-oriented transports MUST ignore any CRLF appearing before the
    % start-line
    parse_stream_internal({'BEFORE', Rest}, 0);
parse_stream_internal({'BEFORE', Frame}, _From) when Frame =:= <<"\r">>; Frame =:= <<>> ->
    % frame is empty or "\r" while ignoring \r\n, return same state
    {ok, {'BEFORE', Frame}};
parse_stream_internal({State, Frame}, From) when State =:= 'HEADERS'; State =:= 'BEFORE'->
    % Search if header-body delimiter is present
    % We need to look back 3 characters at most
    % (last frame ends with \r\n\r, we have received \n)
    case has_header_delimiter(Frame, From - 3) of
        false -> {ok, {'HEADERS', Frame}};
        Pos ->
            % Split packet into headers and the rest
            Pos2 = Pos + 2,
            <<Top:Pos2/binary, "\r\n", Rest/binary>> = Frame,
            % Get start line and headers
            [Start, HeadersBin] = binary:split(Top, <<"\r\n">>),
            Headers = sip_headers:parse_headers(HeadersBin),
            Msg = parse_start_line(Start),
            Msg2 = set_headers(Msg, Headers),

            % Check content length present
            case content_length(Headers) of
                {ok, ContentLength} ->
                    % continue processing the message body
                    NewState = {'BODY', Msg2, ContentLength},
                    parse_stream_internal({NewState, Rest}, 0);
                false ->
                    % return bad message
                    Msg3 = set_body(Msg2, Rest),
                    {error, no_content_length, Msg3, {'BEFORE', <<>>}}
            end
    end;
parse_stream_internal({{'BODY', Msg, ContentLength}, Frame}, _)
  when size(Frame) >= ContentLength ->
    % received the whole body
    <<Body:ContentLength/binary, Rest/binary>> = Frame,
    % return parsed message
    {ok, set_body(Msg, Body), {'BEFORE', Rest}};
parse_stream_internal(State, _) ->
    % nothing to parse yet, return current state
    {ok, State}.

content_length(Headers) ->
    case lists:keyfind('content-length', 1, Headers) of
        false -> false;
        {_Name, Length} -> {ok, sip_binary:binary_to_integer(Length)}
    end.

%% Check if we have header-body delimiter in the received packet
has_header_delimiter(Data, Offset) when Offset < 0 ->
    has_header_delimiter(Data, 0);

has_header_delimiter(Data, Offset) ->
    case binary:match(Data, <<"\r\n\r\n">>, [{scope, {Offset, size(Data) - Offset}}]) of
        nomatch -> false;
        {Pos, _} -> Pos
    end.

%% Request-Line   =  Method SP Request-URI SP SIP-Version CRLF
%% Status-Line  =  SIP-Version SP Status-Code SP Reason-Phrase CRLF
%% start-line   =  Request-Line / Status-Line
%%
%% RFC3261 7.1: The SIP-Version string is case-insensitive, but implementations MUST send upper-case.
-spec parse_start_line(binary()) -> #sip_request{} | #sip_response{}.
parse_start_line(StartLine) when is_binary(StartLine) ->
    % split on three parts
    [First, Rest] = binary:split(StartLine, <<" ">>),
    [Second, Third] = binary:split(Rest, <<" ">>),
    case {First, Second, Third} of
        {Method, RequestURI, <<?SIPVERSION>>} ->
            #sip_request{method = sip_syntax:parse_name(sip_binary:to_upper(Method)),
                         uri = RequestURI};

        {<<?SIPVERSION>>, <<A,B,C>>, ReasonPhrase} when
          $1 =< A andalso A =< $6 andalso % 1xx - 6xx
          $0 =< B andalso B =< $9 andalso
          $0 =< C andalso C =< $9 ->
            #sip_response{status = list_to_integer([A, B, C]),
                          reason = ReasonPhrase}
    end.

-spec default_reason(sip_status()) -> binary().
default_reason(Status) ->
    case Status of
        100 -> <<"Trying">>;
        180 -> <<"Ringing">>;
        181 -> <<"Call Is Being Forwarded">>;
        182 -> <<"Queued">>;
        183 -> <<"Session Progress">>;
        200 -> <<"Ok">>;
        300 -> <<"Multiple Choices">>;
        301 -> <<"Moved Permanently">>;
        302 -> <<"Moved Temporarily">>;
        305 -> <<"Use Proxy">>;
        380 -> <<"Alternative Service">>;
        400 -> <<"Bad Request">>;
        401 -> <<"Unauthorized">>;
        402 -> <<"Payment Required">>;
        403 -> <<"Forbidden">>;
        404 -> <<"Not Found">>;
        405 -> <<"Method Not Allowed">>;
        406 -> <<"Not Acceptable">>;
        407 -> <<"Proxy Authentication Required">>;
        408 -> <<"Request Timeout">>;
        410 -> <<"Gone">>;
        413 -> <<"Request Entity Too Large">>;
        414 -> <<"Request-URI Too Long">>;
        415 -> <<"Unsupported Media Type">>;
        416 -> <<"Unsupported URI Scheme">>;
        420 -> <<"Bad Extension">>;
        421 -> <<"Extension Required">>;
        423 -> <<"Interval Too Brief">>;
        480 -> <<"Temporarily Unavailable">>;
        481 -> <<"Call/Transaction Does Not Exist">>;
        482 -> <<"Loop Detected">>;
        483 -> <<"Too Many Hops">>;
        484 -> <<"Address Incomplete">>;
        485 -> <<"Ambiguous">>;
        486 -> <<"Busy Here">>;
        487 -> <<"Request Terminated">>;
        488 -> <<"Not Acceptable Here">>;
        491 -> <<"Request Pending">>;
        493 -> <<"Undecipherable">>;
        500 -> <<"Server Internal Error">>;
        501 -> <<"Not Implemented">>;
        502 -> <<"Bad Gateway">>;
        503 -> <<"Service Unavailable">>;
        504 -> <<"Server Time-out">>;
        505 -> <<"Version Not Supported">>;
        513 -> <<"Message Too Large">>;
        600 -> <<"Busy Everywhere">>;
        603 -> <<"Decline">>;
        604 -> <<"Does Not Exist Anywhere">>;
        606 -> <<"Not Acceptable">>
    end.

check(true, _Reason) -> ok;
check(false, Reason) -> error_m:fail(Reason).

count(Name, Msg) ->
    length(header_values(Name, Msg)).

is_single_contact(Msg) ->
    case header_values(contact, Msg) of
        [Contact] when is_record(Contact#sip_hdr_address.uri, sip_uri) -> true;
        _Other -> false % either no contact header or multiple headers
    end.

is_contact_secure(Request) ->
    IsRequestSecure = sip_uri:is_sips(Request#sip_request.uri),
    IsRouteSecure =
        case has_header(route, Request) of
            true ->
                #sip_hdr_address{uri = Route} = header_top_value(route, Request),
                sip_uri:is_sips(Route);
            false -> false
        end,
    case has_header(contact, Request) of
        true ->
            Contact = header_top_value(contact, Request),
            IsContactSecure = sip_uri:is_sips(Contact#sip_hdr_address.uri),

            % Contact can be non-secure only if both Request-URI and top Route are not secure, 12.1.2
            IsContactSecure orelse (not IsRequestSecure andalso not IsRouteSecure);
        false ->
            true % no Contact header, so no check
    end.

%% @doc Retrieve headers from SIP request/response
%% @end
-spec headers(sip_message()) -> sip_headers().
headers(#sip_request{headers = Headers}) -> Headers;
headers(#sip_response{headers = Headers}) -> Headers.

%% @doc Set headers to SIP request/response
%% @end
-spec set_headers(sip_message(), sip_headers()) -> sip_message().
set_headers(#sip_request{} = Msg, Headers) -> Msg#sip_request{headers = Headers};
set_headers(#sip_response{} = Msg, Headers) -> Msg#sip_response{headers = Headers}.

%% @doc Retrieve body from SIP request/response
%% @end
-spec body(sip_message()) -> binary().
body(#sip_request{body = Body}) -> Body;
body(#sip_response{body = Body}) -> Body.

%% @doc Set body to SIP request/response
%% @end
-spec set_body(sip_message(), binary()) -> sip_message().
set_body(#sip_request{} = Msg, Body)
  when is_binary(Body) -> Msg#sip_request{body = Body};
set_body(#sip_response{} = Msg, Body)
  when is_binary(Body) -> Msg#sip_response{body = Body}.

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec parse_stream_test_() -> term().
parse_stream_test_() ->
    SampleRequest = #sip_request{method = 'INVITE',
                                 uri = <<"sip:urn:service:test">>,
                                 headers = [{'content-length', <<"5">>}],
                                 body = <<"Hello">>},
    [%% Skipping \r\n
     ?_assertEqual({ok, {'BEFORE', <<>>}},
                   parse_stream(<<>>, none)),
     ?_assertEqual({ok, {'BEFORE', <<>>}},
                   parse_stream(<<"\r\n">>, none)),
     ?_assertEqual({ok, {'BEFORE', <<"\r">>}},
                   parse_stream(<<"\r">>, none)),

     % Test headers-body delimiter test
     ?_assertEqual({ok, {'HEADERS', <<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>, none)),

     ?_assertEqual({ok, {{'BODY', SampleRequest#sip_request{body = <<>>}, 5}, <<>>}},
                   parse_stream(<<"\n">>,
                                {'HEADERS', <<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>})),

     ?_assertEqual({ok, {{'BODY', SampleRequest#sip_request{body = <<>>}, 5}, <<"He">>}},
                   parse_stream(<<"He">>,
                                {{'BODY', SampleRequest#sip_request{body = <<>>}, 5}, <<>>})),

     % Parse the whole body
     ?_assertEqual({ok, SampleRequest, {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>, none)),
     ?_assertEqual({ok, SampleRequest, {'BEFORE', <<>>}},
                   parse_stream(<<"Hello">>,
                                {{'BODY', SampleRequest#sip_request{body = <<>>}, 5}, <<>>})),
     ?_assertEqual({ok,
                    SampleRequest#sip_request{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello">>,
                                {'BEFORE', <<>>})),

     % Multiple messages in stream
     ?_assertEqual({ok, SampleRequest, {'BEFORE', <<"\r\nINVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello\r\nINVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>, none)),

     % No Content-Length
     ?_assertEqual({error,
                    no_content_length,
                    SampleRequest#sip_request{headers = [{<<"x-custom">>, <<"Nothing">>}],
                                              body = <<"Hello">>},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>,
                                {'BEFORE', <<>>})),
     ?_assertEqual({error,
                    no_content_length,
                    #sip_response{status = 200, reason = <<"Ok">>},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"SIP/2.0 200 Ok\r\n\r\n">>,
                                {'BEFORE', <<>>}))
    ].

-spec parse_datagram_test_() -> term().
parse_datagram_test_() ->
    SampleRequest = #sip_request{method = 'INVITE',
                                 uri = <<"sip:urn:service:test">>,
                                 headers = [{'content-length', <<"5">>}],
                                 body = <<"Hello">>},
    SampleResponse = #sip_response{reason = <<"Moved Permanently">>,
                                   status = 301,
                                   headers = [{'content-length', <<"5">>}],
                                   body = <<"Hello">>},
    [
     % Parse the whole body
     ?_assertEqual({ok, SampleRequest},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>)),
     ?_assertEqual({ok, SampleRequest#sip_request{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]}},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello!!!">>)),
     ?_assertEqual({ok, SampleResponse},
                   parse_datagram(<<"SIP/2.0 301 Moved Permanently\r\nContent-Length: 5\r\n\r\nHello">>)),

     % Message too small
     ?_assertEqual({error, content_too_small,
                           #sip_request{method = 'INVITE',
                                        uri = <<"sip:urn:service:test">>,
                                        headers = [{'content-length', <<"10">>}]}},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 10\r\n\r\nHello">>)),
     ?_assertEqual({error, content_too_small,
                           #sip_response{status = 200, reason = <<"Ok">>,
                                         headers = [{'content-length', <<"10">>}]}},
                   parse_datagram(<<"SIP/2.0 200 Ok\r\nContent-Length: 10\r\n\r\n">>)),

     % No Content-Length
     ?_assertEqual({ok, SampleRequest#sip_request{headers = [{<<"x-custom">>, <<"Nothing">>}],
                                                  body = <<"Hello">> } },
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>)),
     ?_assertEqual({ok, #sip_response{status = 200, reason = <<"Ok">>}},
                   parse_datagram(<<"SIP/2.0 200 Ok\r\n\r\n">>))
    ].

-spec format_test_() -> term().
format_test_() ->
    {ok, Request, _} = parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nX-Custom: Nothing\r\n\r\nHello">>, none),
    {ok, Response, _} = parse_stream(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\n\r\nHello">>, none),
    [
     ?_assertEqual(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nx-custom: Nothing\r\n\r\nHello">>, to_binary(Request)),
     ?_assertEqual(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\n\r\nHello">>, to_binary(Response))
    ].

-spec method_test_() -> term().
method_test_() ->
    {ok, Request} = parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nX-Custom: Nothing\r\n\r\nHello">>),
    {ok, Response} = parse_datagram(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\nCSeq: 123 INVITE\r\n\r\nHello">>),
    [?_assertEqual('INVITE', method(Request)),
     ?_assertEqual('INVITE', method(Response))
     ].

-spec create_response_test_() -> list().
create_response_test_() ->
    Request =
        #sip_request{method = 'INVITE', uri = <<"sip:bob@biloxi.com">>,
                     headers = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                                {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
                                {to, <<"Bob <sip:bob@biloxi.com>;tag=XyXULrhOnNkGqswu">>},
                                {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                                {'call-id', <<"987asjd97y7atg">>},
                                {cseq, <<"986759 INVITE">>},
                                {route, <<"<sip:alice@atlanta.com>">>},
                                {route, <<"<sip:bob@biloxi.com>">>}]},

    % (default reason)
    Response =
        #sip_response{status = 405, reason = <<"Method Not Allowed">>,
                     headers = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                                {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
                                {to, <<"Bob <sip:bob@biloxi.com>;tag=XyXULrhOnNkGqswu">>},
                                {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                                {'call-id', <<"987asjd97y7atg">>},
                                {cseq, <<"986759 INVITE">>}]},
    % custom reason
    Response2 = Response#sip_response{status = 201, reason = <<"Very Ok!">>},

    [% Check that all required headers are copied
     ?_assertEqual(Response, create_response(Request, 405)),
     ?_assertEqual(Response2, create_response(Request, 201, <<"Very Ok!">>))
     ].

-spec create_ack_cancel_test_() -> list().
create_ack_cancel_test_() ->
    ReqHeaders = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                  {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
                  {to, <<"Bob <sip:bob@biloxi.com>">>},
                  {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                  {'call-id', <<"987asjd97y7atg">>},
                  {cseq, <<"986759 INVITE">>},
                  {route, <<"<sip:alice@atlanta.com>">>},
                  {route, <<"<sip:bob@biloxi.com>">>},
                  {'content-length', 5},
                  {'max-forwards', 15}],
    Request = #sip_request{method = 'INVITE', uri = <<"sip:bob@biloxi.com">>, headers = ReqHeaders, body = <<"Hello">>},

    Response = create_response(Request, 500),
    Response2 = replace_top_header(to, <<"Bob <sip:bob@biloxi.com>;tag=1928301774">>, Response),

    ACKHeaders = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                  {to, <<"Bob <sip:bob@biloxi.com>;tag=1928301774">>},
                  {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                  {'call-id', <<"987asjd97y7atg">>},
                  {cseq, <<"986759 ACK">>},
                  {route, <<"<sip:alice@atlanta.com>">>},
                  {route, <<"<sip:bob@biloxi.com>">>},
                  {'max-forwards', 15}],

    CancelHeaders = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                  {to, <<"Bob <sip:bob@biloxi.com>">>},
                  {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                  {'call-id', <<"987asjd97y7atg">>},
                  {cseq, <<"986759 CANCEL">>},
                  {route, <<"<sip:alice@atlanta.com>">>},
                  {route, <<"<sip:bob@biloxi.com>">>},
                  {'max-forwards', 15}],

    ACK = #sip_request{method = 'ACK', uri = <<"sip:bob@biloxi.com">>, headers = ACKHeaders},
    Cancel = #sip_request{method = 'CANCEL', uri = <<"sip:bob@biloxi.com">>, headers = CancelHeaders},
    [
     ?_assertEqual(parse_all_headers(ACK), parse_all_headers(create_ack(Request, Response2))),
     ?_assertEqual(parse_all_headers(Cancel), parse_all_headers(create_cancel(Request)))
     ].

-spec helpers_test_() -> list().
helpers_test_() ->

    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"localhost", 5060}, []),

    [% Header lookup functions
     ?_assertEqual(Via1, header_top_value('via', #sip_request{headers = [{'via', [Via1, Via2]}]})),
     ?_assertEqual([Via1, Via2, Via1Up], header_values('via', #sip_request{headers = [{'via', Via1}, {'via', [Via2, Via1Up]}]})),

     ?_assertEqual(true, has_header(via, #sip_request{headers = [{via, [Via1, Via2]}]})),
     ?_assertEqual(false, has_header(via, #sip_request{headers = [{'content-length', 5}]})),

     % Fold Via's (extract hostnames in reverse order)
     ?_assertEqual(["localhost", "127.0.0.2", "127.0.0.1"],
                   foldl_headers(via, fun (#sip_hdr_via{host = Host}, Acc) -> [Host | Acc] end, [],
                                 #sip_request{headers = [{'via', Via1}, {'via', [Via2, Via1Up]}]}))
     ].

-spec validation_test_() -> list().
validation_test_() ->
    URI = <<"sip@nowhere.invalid">>,
    ValidRequest =
        #sip_request{method = 'OPTIONS',
                     uri = URI,
                     headers = [{to, sip_headers:address(<<>>, URI, [])},
                                {from, sip_headers:address(<<>>, URI, [{tag, <<"fromtag">>}])},
                                {cseq, sip_headers:cseq(1, 'OPTIONS')},
                                {'call-id', <<"123">>},
                                % Consider request without Max-Forward as valid, see Issue #7
                                %{'max-forwards', 70},
                                {via, sip_headers:via(udp, "localhost", [])},
                                {require, <<"foo">>}]},
    ValidResponse = create_response(ValidRequest, 200),

    % No contact in dialog-establishing request
    InvalidRequest = replace_top_header(
                       cseq,
                       sip_headers:cseq(1, 'INVITE'),
                       ValidRequest#sip_request{method = 'INVITE'}),

    % with added contact header
    ValidRequest2 = append_header(contact, sip_headers:address(<<>>, <<"sip:alice@localhost">>, []), InvalidRequest),

    % Contact is not sips URI, but top route is
    InvalidRequest2 = append_header(route, sip_headers:address(<<>>, <<"sips:proxy@localhost">>, []), ValidRequest2),
    ValidRequest3 = replace_top_header(contact, sip_headers:address(<<>>, <<"sips:alice@localhost">>, []), InvalidRequest2),

    % Valid response
    ValidResponse = create_response(ValidRequest, 200),

    % Without a Via header
    InvalidResponse = ValidResponse#sip_response{headers = [{Name, Value} || {Name, Value} <- ValidResponse#sip_response.headers, Name =/= via]},

    % Invalid dialog response, no contact
    InvalidResponse2 = create_response(ValidRequest2, 200),
    ValidResponse2 = append_header(contact, sip_headers:address(<<>>, <<"sip:alice@localhost">>, []), InvalidResponse2),

    % Contact is not SIPS
    InvalidResponse3 = ValidResponse2,

    [% Request validation
     ?_assertEqual(ok, validate_request(ValidRequest)),
     ?_assertEqual({error, {invalid, contact}}, validate_request(InvalidRequest)),
     ?_assertEqual(ok, validate_request(ValidRequest2)),
     ?_assertEqual({error, {invalid, contact_must_be_sips}}, validate_request(InvalidRequest2)),
     ?_assertEqual(ok, validate_request(ValidRequest3)),

     % Response validation
     ?_assertEqual(ok, validate_response(ValidResponse)),
     ?_assertEqual({error, {invalid, via}}, validate_response(InvalidResponse)),

     % Contact validation
     ?_assertEqual(ok, validate_contact(false, ValidResponse2)),
     ?_assertEqual({error, {invalid, contact}}, validate_contact(false, InvalidResponse2)),
     ?_assertEqual({error, {invalid, contact_must_be_sips}}, validate_contact(true, InvalidResponse3))
     ].

-spec update_header_test() -> ok.
update_header_test() ->

    CSeq = sip_headers:cseq(110, 'INVITE'),
    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"localhost", 5060}, []),

    % two Via: with one value each
    Msg1 =  #sip_request{headers = [{'content-length', 123}, {via, [Via1]}, {via, [Via2]}]},
    Msg1Up = #sip_request{headers = [{'content-length', 123}, {via, [Via1Up]}, {via, [Via2]}]},

    ?assertEqual(Msg1Up, update_top_header(via, fun (_Value) -> Via1Up end, Msg1)),
    ?assertEqual(Msg1Up, replace_top_header(via, Via1Up, Msg1)),

    % one Via: with two values
    Msg2 = #sip_request{headers = [{'content-length', 123}, {via, [Via1, Via2]}]},
    Msg2Up = #sip_request{headers = [{'content-length', 123}, {via, [Via1Up, Via2]}]},

    ?assertEqual(Msg2Up, update_top_header(via, fun (_Value) -> Via1Up end, Msg2)),
    ?assertEqual(Msg2Up, replace_top_header(via, Via1Up, Msg2)),

    % no Via:
    Msg3 = #sip_request{headers = [{'content-length', 123}, {cseq, CSeq}]},
    Msg3Up = #sip_request{headers = [{'content-length', 123}, {cseq, CSeq}, {via, Via1Up}]},

    ?assertEqual(Msg3Up, update_top_header(via, fun (undefined) -> Via1Up end, Msg3)),
    ?assertEqual(Msg3Up, replace_top_header(via, Via1Up, Msg3)),
    ?assertEqual(Msg3, update_top_header(via, fun (undefined) -> undefined end, Msg3)), % do not add

    % update single-valued header
    Msg4 = #sip_request{headers = [{'content-length', 5}, {cseq, CSeq}]},
    Msg4Up = #sip_request{headers = [{'content-length', 10}, {cseq, CSeq}]},

    ?assertEqual(Msg4Up, update_top_header('content-length', fun (5) -> 10 end, Msg4)),
    ?assertEqual(Msg4Up, replace_top_header('content-length', 10, Msg4)),

    %% append header
    Msg5 = #sip_request{headers = [{'content-length', 5}]},
    Msg5Up = #sip_request{headers = [{'content-length', 5}, {cseq, CSeq}]},

    ?assertEqual(Msg5Up, append_header(cseq, CSeq, Msg5)),
    ?assertEqual(Msg5, append_header(via, [], Msg5)), % empty value
    ok.

-spec branch_helpers_test_() -> list().
branch_helpers_test_() ->

    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bKkjshdyff">>}]),
    [% updating branch
     ?_assertEqual(#sip_request{method = 'INVITE', uri = <<"sip@nowhere.invalid">>,
                                headers = [{via, [Via1Up, Via2]}]},
                   with_branch(<<"z9hG4bKkjshdyff">>,
                               #sip_request{method = 'INVITE', uri = <<"sip@nowhere.invalid">>,
                                            headers = [{via, [Via1, Via2]}]}))
     ].

-spec is_dialog_establishing_test_() -> list().
is_dialog_establishing_test_() ->
    Headers = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
               {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
               {to, <<"Bob <sip:bob@biloxi.com>">>},
               {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
               {'call-id', <<"987asjd97y7atg">>},
               {cseq, <<"986759 INVITE">>},
               {route, <<"<sip:alice@atlanta.com>">>},
               {route, <<"<sip:bob@biloxi.com>">>},
               {'content-length', 5},
               {'max-forwards', 15}],
    Request = #sip_request{method = 'INVITE', uri = <<"sip:bob@biloxi.com">>, headers = Headers, body = <<"Hello">>},

    ToWithTag = sip_headers:address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{tag, <<"1928301774">>}]),
    [
     ?_assertEqual(true, is_dialog_establishing(Request)),
     ?_assertEqual(false, is_dialog_establishing(Request#sip_request{method = 'OPTIONS'})),
     ?_assertEqual(false, is_dialog_establishing(replace_top_header(to, ToWithTag, Request)))
     ].


-spec is_secure_test_() -> list().
is_secure_test_() ->
    Headers = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
               {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
               {to, <<"Bob <sip:bob@biloxi.com>">>},
               {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
               {'call-id', <<"987asjd97y7atg">>},
               {cseq, <<"986759 INVITE">>},
               {contact, <<"<sip:alice@atlanta.com>">>},
               {route, <<"<sip:alice@atlanta.com>">>},
               {route, <<"<sip:bob@biloxi.com>">>},
               {'content-length', 5},
               {'max-forwards', 15}],
    Request = #sip_request{method = 'INVITE', uri = sip_uri:parse(<<"sip:bob@biloxi.com">>), headers = Headers, body = <<"Hello">>},

    SecureURI = sip_uri:parse(<<"sips:bob@biloxi.com">>),
    SecureContact = sip_headers:address(<<"Alice">>, <<"sips:alice@atlanta.com">>, []),
    SecureRecordRoute = sip_headers:address(<<>>, <<"sips:proxy@biloxi.com">>, []),
    RecordRoute = sip_headers:address(<<>>, <<"sip:proxy@biloxi.com">>, []),
    io:format("Request ~p~n", [replace_top_header(contact, <<"Alice <sips:alice@atlanta.com>">>, Request)]),
    [
     ?_assertEqual(false, is_secure(Request)),
     ?_assertEqual(true, is_secure(Request#sip_request{uri = SecureURI})),
     ?_assertEqual(true, is_secure(replace_top_header(contact, SecureContact, Request))),
     ?_assertEqual(true, is_secure(append_header('record-route', SecureRecordRoute, Request))),
     % Contact is secure, but we look at top Record-Route
     ?_assertEqual(false, is_secure(append_header('record-route', RecordRoute, replace_top_header(contact, SecureContact, Request))))
     ].


-spec fake_test() -> ok.
fake_test() ->
    % fake test to create coverage for default_reason function
    Fun = fun(700, _Self) -> ok;
             (Code, Self) -> catch default_reason(Code), Self(Code + 1, Self)
          end,
    % simply call it with all codes from 100 to 700
    Fun(100, Fun).

-endif.
