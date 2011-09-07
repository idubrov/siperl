%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP messages parsing/generation.
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_message).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
-export([is_request/1, is_response/1, method/1]).
-export([parse_stream/2, parse_datagram/1, parse_all_headers/1, to_binary/1]).
-export([create_ack/2, create_response/2, create_response/3]).
-export([validate_request/1]).
-export([update_top_header/3, replace_top_header/3, append_header/3]).
-export([header_values/2, top_header/2, top_via_branch/1, tag/2]).
-export([with_branch/2, foldl_headers/4]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("sip_common.hrl").
-include("sip_syntax.hrl").
-include("sip.hrl").

%% Types

%% Internal state
%% 'BEFORE' -- state before Start-Line
%% 'HEADERS' -- state after first Start-Line character was received
%% {'BODY', Message, Length} -- state after receiving headers, but before body (\r\n\r\n)
-type state() :: {'BEFORE' | 'HEADERS' | {'BODY', #sip_message{}, integer()}, binary()}.

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc Check if message is SIP request
%% @end
-spec is_request(#sip_message{}) -> boolean().
is_request(#sip_message{kind = #sip_request{}}) -> true;
is_request(#sip_message{kind = #sip_response{}}) -> false.

%% @doc Check if message is SIP response
%% @end
-spec is_response(#sip_message{}) -> boolean().
is_response(Message) ->
    not is_request(Message).

%% @doc Retrieve method of SIP message
%%
%% Returns `Method' from `start-line' for requests, `Method' from `CSeq' header
%% for responses.
%% @end
-spec method(#sip_message{}) -> atom() | binary().
method(#sip_message{kind = #sip_request{method = Method}}) -> Method;
method(#sip_message{kind = #sip_response{}} = Msg) ->
    {ok, CSeq} = sip_message:top_header(cseq, Msg),
    CSeq#sip_hdr_cseq.method.

-spec to_binary(#sip_message{}) -> binary().
to_binary(Message) ->
    Top = case Message#sip_message.kind of
              #sip_request{method = Method, uri = URI} ->
                  URIBin = sip_uri:format(URI),
                  <<(sip_syntax:format_name(Method))/binary, " ", URIBin/binary, " ", ?SIPVERSION>>;
              #sip_response{status = Status, reason = Reason} ->
                  StatusStr = list_to_binary(integer_to_list(Status)),
                  <<?SIPVERSION, " ", StatusStr/binary, " ", Reason/binary>>
          end,
    Headers = sip_headers:format_headers(Message#sip_message.headers),
    iolist_to_binary([Top, <<"\r\n">>, Headers, <<"\r\n">>, Message#sip_message.body]).


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
        atom() | binary(),
        fun((Value :: any()) -> UpdatedValue :: any()),
        #sip_message{}) -> #sip_message{}.
update_top_header(HeaderName, Fun, Request) ->
    Headers = update_header(HeaderName, Fun, Request#sip_message.headers),
    Request#sip_message{headers = Headers}.

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
-spec replace_top_header(atom() | binary(), term() | binary(), #sip_message{}) -> #sip_message{}.
replace_top_header(HeaderName, Value, Message) ->
    update_top_header(HeaderName, fun (_Old) -> Value end, Message).

%% @doc Append header with given name and value
%%
%% Appends header with given name and value to the end of the headers list.
%% <em>If header value is empty list, message is not modified</em>
%% @end
-spec append_header(atom() | binary(), term() | binary(), #sip_message{}) -> #sip_message{}.
append_header(_HeaderName, [], Message) when is_record(Message, sip_message) ->
    Message;
append_header(HeaderName, Value, Message) when is_record(Message, sip_message) ->
    Message#sip_message{headers = Message#sip_message.headers ++ [{HeaderName, Value}]}.

%% @doc
%% Retrieve `branch' parameter of top via header or `undefined' if no such
%% parameter present.
%%
%% This function parses the Via: header value if header is in binary form.
%% @end
-spec top_via_branch(#sip_message{}) -> {ok, binary()} | {error, not_found}.
top_via_branch(Message) when is_record(Message, sip_message) ->
    {ok, Via} = top_header('via', Message#sip_message.headers),
    case lists:keyfind(branch, 1, Via#sip_hdr_via.params) of
        {branch, Branch} -> {ok, Branch};
        false -> {error, not_found}
    end.

%% @doc Update top `Via:' header of given request with provided branch value
%% @end
-spec with_branch(binary(), #sip_message{}) -> #sip_message{}.
with_branch(Branch, #sip_message{kind = #sip_request{}} = Msg) when is_binary(Branch) ->
    UpdateBranch =
        fun (Via) ->
                 Params = lists:keystore(branch, 1, Via#sip_hdr_via.params, {branch, Branch}),
                 Via#sip_hdr_via{params = Params}
        end,
    update_top_header('via', UpdateBranch, Msg).

%% @doc Retrieve `tag' parameter of From/To header
%%
%% Retrieve `tag' parameter of `From:'/`To:' header or `undefined' if no such
%% parameter present.
%%
%% This function parses the header value if header is in binary form.
%% @end
-spec tag('to' | 'from', #sip_message{}) -> {value, binary()} | false.
tag(Header, Message) when
  is_record(Message, sip_message), (Header =:= 'to' orelse Header =:= 'from') ->
    {ok, Value} = top_header(Header, Message#sip_message.headers),
    case lists:keyfind(tag, 1, Value#sip_hdr_address.params) of
        {tag, Tag} when is_binary(Tag) -> {value, Tag};
        false -> false
    end.

%% @doc Calls `Fun(Value, AccIn)' on all successive header values named `Name'
%%
%% <em>Note: this function parses the header value if header is in binary form.</em>
%% @end
-spec foldl_headers(atom() | binary(),
                    fun ((Value::term(), AccIn::term()) -> AccOut :: term()),
                    term(),
                    #sip_message{}) -> Acc :: term().
foldl_headers(Name, Fun, Acc0, Msg) when is_function(Fun, 2), is_record(Msg, sip_message) ->
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
-spec header_values(atom() | binary(), #sip_message{} | [{Name :: atom() | binary(), Value :: binary() | term()}]) ->
          [any()].
header_values(Name, Message) when is_record(Message, sip_message) ->
    header_values(Name, Message#sip_message.headers);
header_values(Name, Headers) when is_list(Headers) ->
    Filtered = lists:filter(fun ({N, _Value}) -> N =:= Name end, Headers),
    Parsed = lists:map(fun ({_Name, Value}) -> sip_headers:parse(Name, Value) end, Filtered),
    lists:flatten(Parsed).

%% @doc
%% Retrieve top value of given header. Accepts either full SIP message
%% or list of headers.
%%
%% <em>This function parses the header value if header is in binary form.</em>
%% @end
-spec top_header(atom() | binary(), #sip_message{} | [{Name :: atom() | binary(), Value :: binary() | term()}]) ->
          {ok, term()} | {error, not_found}.
top_header(Name, Message) when is_record(Message, sip_message) ->
    top_header(Name, Message#sip_message.headers);
top_header(Name, Headers) when is_list(Headers) ->
    case lists:keyfind(Name, 1, Headers) of
        false -> {error, not_found};
        {Name, Value} ->
            case sip_headers:parse(Name, Value) of
                [Top | _] -> {ok, Top}; % support for multiple header values
                Top -> {ok, Top}
            end
    end.


%% @doc
%% Parses the datagram for SIP packet. The headers of the returned message are
%% retained in binary form for performance reasons. Use {@link parse_whole/1}
%% to parse the whole message or {@link sip_headers:parse/2} to parse
%% single header.
%% @end
-spec parse_datagram(Datagram :: binary()) ->
          {ok, #sip_message{}}
        | {error, content_too_small, #sip_message{}}.
parse_datagram(Datagram) ->
    {Pos, _Length} = binary:match(Datagram, <<"\r\n\r\n">>),
    Pos2 = Pos + 2,
    <<Top:Pos2/binary, "\r\n", Body/binary>> = Datagram,
    [Start, HeadersBin] = binary:split(Top, <<"\r\n">>),
    Headers = sip_headers:parse_headers(HeadersBin),
    Kind = parse_start_line(Start),

    % RFC 3261 18.3
    case top_header('content-length', Headers) of
        % Content-Length is present
        {ok, ContentLength} when ContentLength =< size(Body) ->
            <<Body2:ContentLength/binary, _/binary>> = Body,
            {ok, #sip_message{kind = Kind, headers = Headers, body = Body2}};
        {ok, _} ->
            {error, content_too_small, #sip_message{kind = Kind, headers = Headers, body = <<>>}};
        % Content-Length is not present
        {error, not_found} ->
            {ok, #sip_message{kind = Kind, headers = Headers, body = Body}}
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
          {ok, #sip_message{}, state()} |
          {error, no_content_length, #sip_message{}, state()}.
parse_stream(Packet, none) -> parse_stream(Packet, {'BEFORE', <<>>});
parse_stream(Packet, {State, Frame}) when is_binary(Packet) ->
    NewFrame = <<Frame/binary, Packet/binary>>,
    parse_stream_internal({State, NewFrame}, size(Frame)).

%% @doc
%% Parses all headers of the message.
%% @end
-spec parse_all_headers(#sip_message{}) -> #sip_message{}.
parse_all_headers(Msg) when is_record(Msg, sip_message) ->
    Headers = [{Name, sip_headers:parse(Name, Value)} || {Name, Value} <- Msg#sip_message.headers],
    Msg#sip_message{headers = Headers}.

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
            Kind = parse_start_line(Start),

            % Check content length present
            case top_header('content-length', Headers) of
                {ok, ContentLength} ->
                    % continue processing the message body
                    Msg = #sip_message{kind = Kind, headers = Headers},
                    NewState = {'BODY', Msg, ContentLength},
                    parse_stream_internal({NewState, Rest}, 0);
                {error, not_found} ->
                    % return bad message
                    Msg = #sip_message{kind = Kind, headers = Headers, body = Rest},
                    {error, no_content_length, Msg, {'BEFORE', <<>>}}
            end
    end;
parse_stream_internal({{'BODY', Msg, ContentLength}, Frame}, _)
  when size(Frame) >= ContentLength ->
    % received the whole body
    <<Body:ContentLength/binary, Rest/binary>> = Frame,
    % return parsed message
    {ok, Msg#sip_message{body = Body}, {'BEFORE', Rest}};
parse_stream_internal(State, _) ->
    % nothing to parse yet, return current state
    {ok, State}.

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

%% @doc
%% RFC 3261, 17.1.1.3 Construction of the ACK Request
%% @end
-spec create_ack(#sip_message{}, #sip_message{}) -> #sip_message{}.
create_ack(Request, Response) when is_record(Request, sip_message),
                                   is_record(Response, sip_message) ->
    #sip_request{method = Method, uri = RequestURI} = Request#sip_message.kind,

    % Call-Id, From, CSeq (with method changed to 'ACK') and Route (for 'INVITE'
    % response ACKs) are taken from the original request
    FoldFun = fun ({'call-id', _} = H, List) -> [H|List];
                  ({'from', _} = H, List) -> [H|List];
                  ({cseq, Value}, List) ->
                       CSeq = sip_headers:parse(cseq, Value),
                       CSeq2 = CSeq#sip_hdr_cseq{method = 'ACK'},
                       [{cseq, CSeq2} | List];
                  ({'route', _} = H, List) when Method =:= 'INVITE' -> [H|List];
                  (_, List) -> List
           end,
    ReqHeaders = lists:reverse(lists:foldl(FoldFun, [], Request#sip_message.headers)),

    % Via is taken from top Via of the original request
    {ok, Via} = top_header('via', Request),

    % To goes from the response
    {ok, To} = top_header('to', Response),

    #sip_message{kind = #sip_request{method = 'ACK', uri = RequestURI},
                 body = <<>>,
                 headers = [{'via', [Via]}, {'to', To} | ReqHeaders]}.


%% @doc Create response for given request
%% FIXME: Copy all needed headers!!!! See Section 20.
%% @end
-spec create_response(#sip_message{}, integer(), binary()) -> #sip_message{}.
create_response(Request, Status, Reason) ->
    Headers = [{Name, Value} || {Name, Value} <- Request#sip_message.headers,
                                (Name =:= 'from' orelse Name =:= 'call-id' orelse
                                 Name =:= cseq orelse Name =:= 'via' orelse
                                 Name =:= 'to')],
    Kind = #sip_response{status = Status, reason = Reason},
    % also, insert zero-length Content-Length, so response could be sent "as-is"
    #sip_message{kind = Kind, headers = [{'content-length', 0} | Headers]}.

%% @doc Create response for given request (with default Reason)
%% @end
-spec create_response(#sip_message{}, integer()) -> #sip_message{}.
create_response(Request, Status) ->
    create_response(Request, Status, default_reason(Status)).

%% @doc Validate that request contains all required headers
%% A valid SIP request formulated by a UAC MUST, at a minimum, contain
%% the following header fields: To, From, CSeq, Call-ID, Max-Forwards,
%% and Via; all of these header fields are mandatory in all SIP
%% requests.
%% @end
-spec validate_request(#sip_message{}) -> ok | {error, Reason :: term()}.
validate_request(Request) when is_record(Request, sip_message) ->
    Method = Request#sip_message.kind#sip_request.method,
    CountFun =
        fun ({Name, Value}, Counts) ->
                 % assign tuple index for every header being counted
                 Idx = case Name of
                           to -> 1;
                           from -> 2;
                           cseq -> 3;
                           'call-id' -> 4;
                           'max-forwards' -> 5;
                           via -> 6;
                           contact -> 7;
                           _ -> 0
                       end,
                 if
                     Idx > 0 ->
                         Incr = if is_list(Value) -> length(Value); true -> 1 end,
                         setelement(Idx, Counts, element(Idx, Counts) + Incr);
                     true -> Counts
                 end
        end,

    % Count headers
    case lists:foldl(CountFun, {0, 0, 0, 0, 0, 0, 0}, Request#sip_message.headers) of
        C when C >= {1, 1, 1, 1, 1, 1, 0}, % Each header must be at least once (except contact),
               C =< {1, 1, 1, 1, 1, a, 0}, % except Via:, which must be at least once (atom > every possible number)

               % The Contact header field MUST be present and contain exactly one SIP
               % or SIPS URI in any request that can result in the establishment of a
               % dialog.
               (Method =/= 'INVITE' orelse element(7, C) =:= 1)
          -> ok;
        _ -> {error, invalid_headers}
    end.

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

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec parse_stream_test_() -> term().
parse_stream_test_() ->
    SampleRequest = #sip_request{method = 'INVITE', uri = <<"sip:urn:service:test">>},
    SampleMessage = #sip_message{kind = SampleRequest,
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

     ?_assertEqual({ok, {{'BODY', SampleMessage#sip_message{body = <<>>}, 5}, <<>>}},
                   parse_stream(<<"\n">>,
                                {'HEADERS', <<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>})),

     ?_assertEqual({ok, {{'BODY', SampleMessage#sip_message{body = <<>>}, 5}, <<"He">>}},
                   parse_stream(<<"He">>,
                                {{'BODY', SampleMessage#sip_message{body = <<>>}, 5}, <<>>})),

     % Parse the whole body
     ?_assertEqual({ok, SampleMessage, {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>, none)),
     ?_assertEqual({ok, SampleMessage, {'BEFORE', <<>>}},
                   parse_stream(<<"Hello">>,
                                {{'BODY', SampleMessage#sip_message{body = <<>>}, 5}, <<>>})),
     ?_assertEqual({ok,
                    SampleMessage#sip_message{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello">>,
                                {'BEFORE', <<>>})),

     % Multiple messages in stream
     ?_assertEqual({ok, SampleMessage, {'BEFORE', <<"\r\nINVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello\r\nINVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>, none)),

     % No Content-Length
     ?_assertEqual({error,
                    no_content_length,
                    #sip_message{kind = SampleRequest,
                                 headers = [{<<"x-custom">>, <<"Nothing">>}],
                                 body = <<"Hello">>},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>,
                                {'BEFORE', <<>>})),
     ?_assertEqual({error,
                    no_content_length,
                    #sip_message{kind = #sip_response{status = 200, reason = <<"Ok">>}},
                    {'BEFORE', <<>>}},
                   parse_stream(<<"SIP/2.0 200 Ok\r\n\r\n">>,
                                {'BEFORE', <<>>}))
    ].

-spec parse_datagram_test_() -> term().
parse_datagram_test_() ->
    SampleRequest = #sip_request{method = 'INVITE', uri = <<"sip:urn:service:test">>},
    SampleMessage = #sip_message{kind = SampleRequest,
                                 headers = [{'content-length', <<"5">>}],
                                 body = <<"Hello">>},
    SampleResponse = #sip_response{reason = <<"Moved Permanently">>, status = 301},
    SampleResponseMessage = #sip_message{kind = SampleResponse,
                                         headers = [{'content-length', <<"5">>}],
                                         body = <<"Hello">>},
    [
     % Parse the whole body
     ?_assertEqual({ok, SampleMessage},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>)),
     ?_assertEqual({ok, SampleMessage#sip_message{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]}},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello!!!">>)),
     ?_assertEqual({ok, SampleResponseMessage},
                   parse_datagram(<<"SIP/2.0 301 Moved Permanently\r\nContent-Length: 5\r\n\r\nHello">>)),

     % Message too small
     ?_assertEqual({error, content_too_small,
                           #sip_message{kind = SampleRequest,
                                        headers = [{'content-length', <<"10">>}]}},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 10\r\n\r\nHello">>)),
     ?_assertEqual({error, content_too_small,
                           #sip_message{kind = #sip_response{status = 200, reason = <<"Ok">>},
                                        headers = [{'content-length', <<"10">>}]}},
                   parse_datagram(<<"SIP/2.0 200 Ok\r\nContent-Length: 10\r\n\r\n">>)),

     % No Content-Length
     ?_assertEqual({ok, #sip_message{kind = SampleRequest,
                                     headers = [{<<"x-custom">>, <<"Nothing">>}],
                                     body = <<"Hello">> } },
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>)),
     ?_assertEqual({ok, #sip_message{kind = #sip_response{status = 200, reason = <<"Ok">>} } },
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


-spec is_test_() -> term().
is_test_() ->
    {ok, Request, _} = parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nX-Custom: Nothing\r\n\r\nHello">>, none),
    {ok, Response, _} = parse_stream(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\n\r\nHello">>, none),
    [?_assertEqual(true, is_request(Request)),
     ?_assertEqual(false, is_request(Response)),
     ?_assertEqual(false, is_response(Request)),
     ?_assertEqual(true, is_response(Response))
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
        #sip_message{kind = #sip_request{method = 'INVITE', uri = <<"sip:bob@biloxi.com">>},
                     headers = [{via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                                {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
                                {to, <<"Bob <sip:bob@biloxi.com>">>},
                                {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                                {'call-id', <<"987asjd97y7atg">>},
                                {cseq, <<"986759 INVITE">>},
                                {route, <<"<sip:alice@atlanta.com>">>},
                                {route, <<"<sip:bob@biloxi.com>">>}]},

    % (default reason)
    Response =
        #sip_message{kind = #sip_response{status = 405, reason = <<"Method Not Allowed">>},
                     headers = [{'content-length', 0},
                                {via, <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                                {via, <<"SIP/2.0/UDP bob.biloxi.com;branch=z9hG4bK776asdhds">>},
                                {to, <<"Bob <sip:bob@biloxi.com>">>},
                                {from, <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                                {'call-id', <<"987asjd97y7atg">>},
                                {cseq, <<"986759 INVITE">>}]},
    % custom reason
    Response2 = Response#sip_message{kind = #sip_response{status = 201, reason = <<"Very Ok!">>}},
    [% Check that all required headers are copied
     % Also, create_response adds content-length of 0
     % XXX: Should the content-length be added by transport layer instead?
     ?_assertEqual(Response, create_response(Request, 405)),
     ?_assertEqual(Response2, create_response(Request, 201, <<"Very Ok!">>))
     ].


-spec create_ack_test_() -> list().
create_ack_test_() ->
    ReqHeaders = [
                  {'via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                  {'to', <<"Bob <sip:bob@biloxi.com>">>},
                  {'from', <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                  {'call-id', <<"987asjd97y7atg">>},
                  {cseq, <<"986759 INVITE">>},
                  {'route', <<"<sip:alice@atlanta.com>">>},
                  {'route', <<"<sip:bob@biloxi.com>">>}
                  ],
    OrigRequest = #sip_message{kind = #sip_request{method = 'INVITE', uri = <<"sip:bob@biloxi.com">>}, headers = ReqHeaders},

    RespHeaders = lists:keyreplace('to', 1, ReqHeaders, {'to', <<"Bob <sip:bob@biloxi.com>;tag=1928301774">>}),
    Response = #sip_message{kind = #sip_response{status = 500, reason = <<"Internal error">>}, headers = RespHeaders},

    ACKHeaders = lists:keyreplace(cseq, 1, RespHeaders, {cseq, <<"986759 ACK">>}),
    ACK = #sip_message{kind = #sip_request{method = 'ACK', uri = <<"sip:bob@biloxi.com">>}, headers = ACKHeaders},
    [
     ?_assertEqual(parse_all_headers(ACK), parse_all_headers(create_ack(OrigRequest, Response)))
     ].

-spec helpers_test_() -> list().
helpers_test_() ->

    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"localhost", 5060}, []),

    URI = <<"sip@nowhere.invalid">>,
    ValidRequest =
        #sip_message{kind = #sip_request{method = 'OPTIONS', uri = URI},
                     headers = [{to, sip_headers:address(<<>>, URI, [])},
                                {from, sip_headers:address(<<>>, URI, [{tag, <<"fromtag">>}])},
                                {cseq, sip_headers:cseq(1, 'OPTIONS')},
                                {'call-id', <<"123">>},
                                {'max-forwards', 70},
                                {via, sip_headers:via(udp, "localhost", [])},
                                {require, <<"foo">>}]},

    % No contact in INVITE
    InvalidRequest =
        ValidRequest#sip_message{kind = #sip_request{method = 'INVITE', uri = URI}},
    % with added contact header
    ValidRequest2 =
        InvalidRequest#sip_message{headers = [{contact, <<"sip:alice@localhost">>} | InvalidRequest#sip_message.headers]},
    [% Header lookup functions
     ?_assertEqual({ok, Via1}, top_header('via', [{'via', [Via1, Via2]}])),
     ?_assertEqual([Via1, Via2, Via1Up], header_values('via', #sip_message{headers = [{'via', Via1}, {'via', [Via2, Via1Up]}]})),

     % Fold Via's (extract hostnames in reverse order)
     ?_assertEqual(["localhost", "127.0.0.2", "127.0.0.1"],
                   foldl_headers(via, fun (#sip_hdr_via{host = Host}, Acc) -> [Host | Acc] end, [],
                                 #sip_message{headers = [{'via', Via1}, {'via', [Via2, Via1Up]}]})),

     % Common parameters lookup functions
     ?_assertEqual({value, <<"fromtag">>}, tag(from, ValidRequest)),
     ?_assertEqual(false, tag(to, ValidRequest)),

     % Validation
     ?_assertEqual(ok, validate_request(ValidRequest)),
     ?_assertEqual({error, invalid_headers}, validate_request(InvalidRequest)),
     ?_assertEqual(ok, validate_request(ValidRequest2))
     ].

-spec update_header_test() -> ok.
update_header_test() ->

    CSeq = sip_headers:cseq(110, 'INVITE'),
    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"localhost", 5060}, []),

    % two Via: with one value each
    Msg1 =  #sip_message{headers = [{'content-length', 123}, {via, [Via1]}, {via, [Via2]}]},
    Msg1Up = #sip_message{headers = [{'content-length', 123}, {via, [Via1Up]}, {via, [Via2]}]},

    ?assertEqual(Msg1Up, update_top_header(via, fun (_Value) -> Via1Up end, Msg1)),
    ?assertEqual(Msg1Up, replace_top_header(via, Via1Up, Msg1)),

    % one Via: with two values
    Msg2 = #sip_message{headers = [{'content-length', 123}, {via, [Via1, Via2]}]},
    Msg2Up = #sip_message{headers = [{'content-length', 123}, {via, [Via1Up, Via2]}]},

    ?assertEqual(Msg2Up, update_top_header(via, fun (_Value) -> Via1Up end, Msg2)),
    ?assertEqual(Msg2Up, replace_top_header(via, Via1Up, Msg2)),

    % no Via:
    Msg3 = #sip_message{headers = [{'content-length', 123}, {cseq, CSeq}]},
    Msg3Up = #sip_message{headers = [{'content-length', 123}, {cseq, CSeq}, {via, Via1Up}]},

    ?assertEqual(Msg3Up, update_top_header(via, fun (undefined) -> Via1Up end, Msg3)),
    ?assertEqual(Msg3Up, replace_top_header(via, Via1Up, Msg3)),
    ?assertEqual(Msg3, update_top_header(via, fun (undefined) -> undefined end, Msg3)), % do not add

    % update single-valued header
    Msg4 = #sip_message{headers = [{'content-length', 5}, {cseq, CSeq}]},
    Msg4Up = #sip_message{headers = [{'content-length', 10}, {cseq, CSeq}]},

    ?assertEqual(Msg4Up, update_top_header('content-length', fun (5) -> 10 end, Msg4)),
    ?assertEqual(Msg4Up, replace_top_header('content-length', 10, Msg4)),

    %% append header
    Msg5 = #sip_message{headers = [{'content-length', 5}]},
    Msg5Up = #sip_message{headers = [{'content-length', 5}, {cseq, CSeq}]},

    ?assertEqual(Msg5Up, append_header(cseq, CSeq, Msg5)),
    ?assertEqual(Msg5, append_header(via, [], Msg5)), % empty value
    ok.

-spec branch_helpers_test_() -> list().
branch_helpers_test_() ->

    Via1 = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {"127.0.0.2", 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {"127.0.0.1", 5060}, [{branch, <<"z9hG4bKkjshdyff">>}]),
    [
     ?_assertEqual({ok, <<"z9hG4bK776asdhds">>},
                   top_via_branch(#sip_message{headers = [{via, [Via1, Via2]}]})),
     ?_assertEqual({error, not_found},
                   top_via_branch(#sip_message{headers = [{via, [Via2]}]})),
     
     % updating branch
     ?_assertEqual(#sip_message{kind = #sip_request{method = 'INVITE', uri = <<"sip@nowhere.invalid">>},
                                headers = [{via, [Via1Up, Via2]}]},
                   with_branch(<<"z9hG4bKkjshdyff">>,
                               #sip_message{kind = #sip_request{method = 'INVITE', uri = <<"sip@nowhere.invalid">>},
                                            headers = [{via, [Via1, Via2]}]}))
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
