%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP messages parsing/generation.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_message).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
-export([is_request/1, is_response/1, to_binary/1]).
-export([is_provisional_response/1, method/1]).
-export([parse_stream/2, parse_datagram/1, parse_all_headers/1, sort_headers/1]).
-export([create_ack/2, create_response/4]).
-export([validate_request/1]).
-export([update_top_header/3, replace_top_header/3]).
-export([top_header/2, top_via_branch/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(SIPVERSION, "SIP/2.0").

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("sip_common.hrl").
-include_lib("sip.hrl").

%% Types

%% Internal state
%% 'BEFORE' -- state before Start-Line
%% 'HEADERS' -- state after first Start-Line character was received
%% {'BODY', StartLine, Headers, Length} -- state after receiving headers, but before body (\r\n\r\n)
-type state() :: {'BEFORE' | 'HEADERS' | {'BODY', start_line(), [sip_headers:header()], integer()}, binary()}.

%% Exported types
-type start_line() :: {'request', Method :: sip_headers:method(), RequestURI :: binary()} |
                      {'response', Status :: integer(), Reason :: binary()}.
-type message() :: #sip_message{}.
-export_type([start_line/0, message/0]).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc
%% Check if message is SIP request.
%% @end
-spec is_request(message()) -> boolean().
is_request(#sip_message{start_line = {request, _, _}}) ->
    true;

is_request(#sip_message{start_line = {response, _, _}}) ->
    false.

%% @doc
%% Check if message is SIP response.
%% @end
-spec is_response(message()) -> boolean().
is_response(Message) ->
    not is_request(Message).

%% @doc
%% Check if message is SIP provisional response (1xx).
%% @end
-spec is_provisional_response(message()) -> boolean().
is_provisional_response(#sip_message{start_line = {response, Status, _}})
  when Status >= 100, Status =< 199 ->
    true;
is_provisional_response(#sip_message{start_line = {response, _Status, _}}) ->
    false.

%% @doc Retrieve method of SIP message
%%
%% Returns `Method' from `start-line' for requests, `Method' from `CSeq' header
%% for responses.
%% @end
-spec method(message()) -> sip_headers:method().
method(#sip_message{start_line = {request, Method, _}}) -> Method;
method(#sip_message{start_line = {response, _, _}} = Msg) ->
    {ok, CSeq} = sip_message:top_header('cseq', Msg),
    CSeq#sip_hdr_cseq.method.

-spec to_binary(message()) -> binary().
to_binary(Message) ->
    Top = case Message#sip_message.start_line of
              {request, Method, URI} -> <<(sip_binary:any_to_binary(Method))/binary, " ", URI/binary, " ", ?SIPVERSION>>;
              {response, Status, Reason} ->
                  StatusStr = list_to_binary(integer_to_list(Status)),
                  <<?SIPVERSION, " ", StatusStr/binary, " ", Reason/binary>>
          end,
    Headers = << <<(sip_headers:to_binary(Name, Value))/binary, "\r\n">> ||
                 {Name, Value} <- Message#sip_message.headers>>,
    iolist_to_binary([Top, <<"\r\n">>, Headers, <<"\r\n">>, Message#sip_message.body]).


%% @doc
%% Update value of top header with given name. If header value is
%% multi-value, only first element of the list is updated by the
%% function.
%%
%% This function parses the header value if header is in binary form.
%%
%% If no header is found with given name, a new one is generated
%% by calling update function with `undefined' header value.
%% @end
-spec update_top_header(
        sip_headers:header_name(),
        fun((Value :: any()) -> UpdatedValue :: any()),
        message()) -> message().
update_top_header(HeaderName, Fun, Request) ->
    Headers = update_header(HeaderName, Fun, Request#sip_message.headers),
    Request#sip_message{headers = Headers}.

%% Internal function to update the header list
update_header(HeaderName, Fun, [{HeaderName, Value} | Rest]) ->
    % Parse header if it is not parsed yet
    UpdatedValue =
        case sip_headers:parse_header(HeaderName, Value) of
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

%% @doc
%% Replace value of top header with given name with provided value. If
%% header value is multi-value (a list), the first element of the list
%% is replaced.
%%
%% <em>Note that header is not added automatically, if there is no header with given name</em>
%% @end
-spec replace_top_header(sip_headers:header_name(), term() | binary(), message()) ->
          message().
replace_top_header(HeaderName, Value, Message) ->
    UpdateFun = fun
                   (undefined) -> undefined; % do not add a new one
                   ([_ | Rest]) -> [Value | Rest];
                   (_) -> Value
                end,
    update_top_header(HeaderName, UpdateFun, Message).

%% @doc
%% Retrieve `branch' parameter of top via header or `undefined' if no such
%% parameter present.
%%
%% This function parses the Via: header value if header is in binary form.
%% @end
-spec top_via_branch(message()) -> {ok, binary()} | {error, not_found}.
top_via_branch(Message) when is_record(Message, sip_message) ->
    {ok, Via} = top_header('via', Message#sip_message.headers),
    case lists:keyfind(branch, 1, Via#sip_hdr_via.params) of
        {branch, Branch} -> {ok, Branch};
        false -> {error, not_found}
    end.

%% @doc
%% Retrieve top value of given header. Accepts either full SIP message
%% or list of headers.
%%
%% This function parses the header value if header is in binary form.
%% @end
-spec top_header(sip_headers:header_name(), message() | [sip_headers:header()]) ->
          {ok, term()} | {error, not_found}.
top_header(Name, Message) when is_record(Message, sip_message) ->
    top_header(Name, Message#sip_message.headers);
top_header(Name, Headers) when is_list(Headers) ->
    case lists:keyfind(Name, 1, Headers) of
        false -> {error, not_found};
        {Name, Value} ->
            case sip_headers:parse_header(Name, Value) of
                [Top | _] -> {ok, Top}; % support for multiple header values
                Top -> {ok, Top}
            end
    end.


%% @doc
%% Parses the datagram for SIP packet. The headers of the returned message are
%% retained in binary form for performance reasons. Use {@link parse_whole/1}
%% to parse the whole message or {@link sip_headers:parse_header/2} to parse
%% single header.
%% @end
-spec parse_datagram(Datagram :: binary()) ->
          {ok, message()}
        | {bad_request, Reason :: term()}
        | {bad_response, Reason :: term()}.
parse_datagram(Datagram) ->
    [Top, Body] = binary:split(Datagram, <<"\r\n\r\n">>),
    [Start | Tail] = binary:split(Top, <<"\r\n">>),
    Headers = case Tail of
                  [] -> [];
                  [Bin] -> sip_headers:split_binary(Bin)
              end,
    StartLine = parse_start_line(Start),

    % RFC 3261 18.3
    case top_header('content-length', Headers) of
        % Content-Length is present
        {ok, ContentLength} when ContentLength =< size(Body) ->
            <<Body2:ContentLength/binary, _/binary>> = Body,
            {ok, #sip_message{start_line = StartLine, headers = Headers, body = Body2}};
        {ok, _} when element(1, StartLine) =:= request ->
            {bad_request, content_too_small};
        {ok, _} ->
            {bad_response, content_too_small};
        % Content-Length is not present
        {error, not_found} ->
            {ok, #sip_message{start_line = StartLine, headers = Headers, body = Body}}
    end.

-spec parse_stream(Packet :: binary(), State :: state() | 'none') ->
          {ok, state(), Msgs :: [message()]}
        | {'bad_request', Reason :: term()}
        | {'bad_response', Reason :: term()}.

%% @doc
%% Parses the stream for complete SIP messages. Return new parser state
%% and list of complete messages extracted from the stream. The headers
%% of the returned messages are retained in binary form for performance
%% reasons. Use {@link parse_whole/1} to parse the whole message or
%% {@link sip_headers:parse_header/2} to parse single header.
%% @end
parse_stream(Packet, none) ->
    parse_stream(Packet, {'BEFORE', <<>>});

parse_stream(Packet, {State, Frame}) when is_binary(Packet) ->
    NewFrame = <<Frame/binary, Packet/binary>>,
    case pre_parse_stream({State, NewFrame}, size(Frame), []) of
        {ok, NewState, Msgs} ->
            {ok, NewState, lists:reverse(Msgs)};

        Error ->
            Error
    end.


%% @doc
%% Parses all headers of the message.
%% @end
-spec parse_all_headers(message()) -> message().
parse_all_headers(Msg) when is_record(Msg, sip_message) ->
    Headers = [{Name, sip_headers:parse_header(Name, Value)} || {Name, Value} <- Msg#sip_message.headers],
    Msg#sip_message{headers = Headers}.

%% @doc
%% Parse and stable sort all headers of the message. This function is mostly used for testing
%% purposes before comparing the messages.
%% @end
-spec sort_headers(message()) -> message().
sort_headers(Msg) when is_record(Msg, sip_message) ->
    Msg2 = parse_all_headers(Msg),
    Msg2#sip_message{headers = lists:keysort(1, Msg2#sip_message.headers)}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% RFC 3261 7.5  Implementations processing SIP messages over
%% stream-oriented transports MUST ignore any CRLF appearing before the
%% start-line
pre_parse_stream({'BEFORE', <<"\r\n", Rest/binary>>}, _, Msgs) ->
    pre_parse_stream({'BEFORE', Rest}, 0, Msgs);

%% Frame is empty or "\r" while ignoring \r\n, return same state
pre_parse_stream({'BEFORE', Frame}, _, Msgs) when Frame =:= <<"\r">>; Frame =:= <<>> ->
    {ok, {'BEFORE', Frame}, Msgs};

%% Look for headers-body delimiter
pre_parse_stream({State, Frame}, From, Msgs) when State =:= 'HEADERS'; State =:= 'BEFORE'->
    % Search if header-body delimiter is present
    % We need to look back 3 characters at most
    % (last frame ends with \r\n\r, we have received \n)
    case has_header_delimiter(Frame, From - 3) of
        false ->
            {ok, {'HEADERS', Frame}, Msgs};

        Pos ->
            % Split packet into headers and the rest
            <<Top:Pos/binary, _:4/binary, Rest/binary>> = Frame,

            % Get start line and headers
            [Start|Tail] = binary:split(Top, <<"\r\n">>),
            Headers = case Tail of
                          [] -> [];
                          [Bin] -> sip_headers:split_binary(Bin)
                      end,
            StartLine = parse_start_line(Start),

            % Check content length present
            case top_header('content-length', Headers) of
                {ok, ContentLength} ->
                    % Continue processing the message body
                    NewState = {'BODY', StartLine, Headers,ContentLength},
                    pre_parse_stream({NewState, Rest}, 0, Msgs);

                {error, not_found} when element(1, StartLine) =:= request ->
                    {bad_request, no_content_length};

                {error, not_found} ->
                    {bad_response, no_content_length}
            end
    end;

%% Check if we have received the whole body
pre_parse_stream({{'BODY', StartLine, Headers, ContentLength}, Frame}, _, Msgs)
  when size(Frame) >= ContentLength ->

    <<Body:ContentLength/binary, Rest/binary>> = Frame,

    % Process the received packet
    NewMsgs = [#sip_message{start_line = StartLine, headers = Headers, body = Body} | Msgs],

    % Continue processing the remaining data as it were a new packet
    pre_parse_stream({'BEFORE', Rest}, 0, NewMsgs);

%% Nothing to parse yet, return current state
pre_parse_stream(State, _, Msgs) ->
    {ok, State, Msgs}.

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
-spec parse_start_line(binary()) -> start_line().
parse_start_line(StartLine) when is_binary(StartLine) ->
    case binary:split(StartLine, <<" ">>, [global]) of
        [Method, RequestURI, <<?SIPVERSION>>]
          ->
            {request, sip_binary:binary_to_existing_atom(sip_binary:to_upper(Method)), RequestURI};

        [<<?SIPVERSION>>, <<A,B,C>>, ReasonPhrase] when
            $1 =< A andalso A =< $6 andalso % 1xx - 6xx
            $0 =< B andalso B =< $9 andalso
            $0 =< C andalso C =< $9
          ->
            {response, list_to_integer([A, B, C]), ReasonPhrase}
    end.

%% @doc
%% RFC 3261, 17.1.1.3 Construction of the ACK Request
%% @end
-spec create_ack(message(), message()) -> message().
create_ack(Request, Response) when is_record(Request, sip_message),
                                   is_record(Response, sip_message) ->
    {request, Method, RequestURI} = Request#sip_message.start_line,

    % Call-Id, From, CSeq (with method changed to 'ACK') and Route (for 'INVITE'
    % response ACKs) are taken from the original request
    FoldFun = fun ({'call-id', _} = H, List) -> [H|List];
                  ({'from', _} = H, List) -> [H|List];
                  ({'cseq', Value}, List) ->
                       CSeq = sip_headers:parse_header('cseq', Value),
                       CSeq2 = CSeq#sip_hdr_cseq{method = 'ACK'},
                       [{'cseq', CSeq2} | List];
                  ({'route', _} = H, List) when Method =:= 'INVITE' -> [H|List];
                  (_, List) -> List
           end,
    ReqHeaders = lists:reverse(lists:foldl(FoldFun, [], Request#sip_message.headers)),

    % Via is taken from top Via of the original request
    {ok, Via} = top_header('via', Request),

    % To goes from the response
    {ok, To} = top_header('to', Response),

    #sip_message{start_line = {request, 'ACK', RequestURI},
                 body = <<>>,
                 headers = [{'via', Via}, {'to', To} | ReqHeaders]}.


%% @doc
%% 8.2.6.1 Sending a Provisional Response
%% FIXME: adding tag...
%% @end
-spec create_response(message(), integer(), binary(), undefined | binary()) -> message().
create_response(Request, Status, Reason, Tag) ->
    Headers = [if
                   Name =:= 'to' -> {'to', sip_headers:add_tag('to', Value, Tag)};
                   true -> {Name, Value}
               end || {Name, Value} <- Request#sip_message.headers,
                      (Name =:= 'from' orelse Name =:= 'call-id' orelse
                       Name =:= 'cseq' orelse Name =:= 'via' orelse
                       Name =:= 'to')],
    Start = {response, Status, Reason},
    #sip_message{start_line = Start, headers = Headers}.

%% @doc Validate that request contains all required headers
%% A valid SIP request formulated by a UAC MUST, at a minimum, contain
%% the following header fields: To, From, CSeq, Call-ID, Max-Forwards,
%% and Via; all of these header fields are mandatory in all SIP
%% requests.
%% @end
-spec validate_request(message()) -> ok | {error, Reason :: term()}.
validate_request(Request) when is_record(Request, sip_message) ->
    {request, Method, _} = Request#sip_message.start_line,
    CountFun =
        fun ({Name, Value}, Counts) ->
                 % assign tuple index for every header being counted
                 Idx = case Name of
                           'to' -> 1;
                           'from' -> 2;
                           'cseq' -> 3;
                           'call-id' -> 4;
                           'max-forwards' -> 5;
                           'via' -> 6;
                           'contact' -> 7;
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

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec parse_request_line_test_() -> term().
parse_request_line_test_() ->
    [?_assertEqual({request, 'INVITE', <<"sip:bob@biloxi.com">>},
                   parse_start_line(<<"INVITE sip:bob@biloxi.com SIP/2.0">>)),
     ?_assertException(error, {case_clause, _}, parse_start_line(<<"INV ITE sip:bob@biloxi.com SIP/2.0">>)),
     ?_assertEqual({response, 200, <<"OK">>},
                   parse_start_line(<<"SIP/2.0 200 OK">>)),
     ?_assertException(error, {case_clause, _}, parse_start_line(<<"SIP/2.0 099 Invalid">>))
    ].

-spec parse_stream_test_() -> term().
parse_stream_test_() ->
    StartState = {'BEFORE', <<>>},
    SampleRequest = {request, 'INVITE', <<"sip:urn:service:test">>},
    SampleMessage = #sip_message{start_line = SampleRequest,
                                 headers = [{'content-length', <<"5">>}],
                                 body = <<"Hello">>},
    [ %% Skipping \r\n
     ?_assertEqual({ok, StartState, []},
                   parse_stream(<<>>, none)),
     ?_assertEqual({ok, StartState, []},
                   parse_stream(<<"\r\n">>, none)),
     ?_assertEqual({ok, {'BEFORE', <<"\r">>}, []},
                   parse_stream(<<"\r">>, none)),

     % Test headers-body delimiter test
     ?_assertEqual({ok, {'HEADERS', <<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>}, []},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>, none)),

     ?_assertEqual({ok, {{'BODY', SampleRequest, [{'content-length', <<"5">>}], 5}, <<>>}, []},
                   parse_stream(<<"\n">>,
                                {'HEADERS', <<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r">>})),

     ?_assertEqual({ok, {{'BODY', SampleRequest, [{'content-length', <<"5">>}], 5}, <<"He">>}, []},
                   parse_stream(<<"He">>,
                                {{'BODY', SampleRequest, [{'content-length', <<"5">>}], 5}, <<>>})),

     % Parse the whole body
     ?_assertEqual({ok, StartState, [SampleMessage]},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>, none)),
     ?_assertEqual({ok, StartState, [SampleMessage]},
                   parse_stream(<<"Hello">>,
                                {{'BODY', SampleRequest, [{'content-length', <<"5">>}], 5}, <<>>})),
     ?_assertEqual({ok, StartState,
                    [SampleMessage#sip_message{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]}]},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello">>,
                                StartState)),

     % No Content-Length
     ?_assertEqual({bad_request, no_content_length},
                   parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>, StartState)),
     ?_assertEqual({bad_response, no_content_length},
                   parse_stream(<<"SIP/2.0 200 Ok\r\n\r\n">>, StartState))
    ].

-spec parse_datagram_test_() -> term().
parse_datagram_test_() ->
    SampleRequest = {request, 'INVITE', <<"sip:urn:service:test">>},
    SampleMessage = #sip_message{start_line = SampleRequest,
                                 headers = [{'content-length', <<"5">>}],
                                 body = <<"Hello">>},
    [
     % Parse the whole body
     ?_assertEqual({ok, SampleMessage},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\n\r\nHello">>)),
     ?_assertEqual({ok, SampleMessage#sip_message{headers = [{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}]}},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\nContent-Length: 5\r\n\r\nHello!!!">>)),

     % Message too small
     ?_assertEqual({bad_request, content_too_small},
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 10\r\n\r\nHello">>)),
     ?_assertEqual({bad_response, content_too_small},
                   parse_datagram(<<"SIP/2.0 200 Ok\r\nContent-Length: 10\r\n\r\n">>)),

     % No Content-Length
     ?_assertEqual({ok, #sip_message{start_line = SampleRequest,
                                     headers = [{<<"x-custom">>, <<"Nothing">>}],
                                     body = <<"Hello">> } },
                   parse_datagram(<<"INVITE sip:urn:service:test SIP/2.0\r\nX-Custom: Nothing\r\n\r\nHello">>)),
     ?_assertEqual({ok, #sip_message{start_line = {response, 200, <<"Ok">>} } },
                   parse_datagram(<<"SIP/2.0 200 Ok\r\n\r\n">>))
    ].

-spec is_test_() -> term().
is_test_() ->
    {ok, _, [Request]} = parse_stream(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nX-Custom: Nothing\r\n\r\nHello">>, none),
    {ok, _, [Response]} = parse_stream(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\n\r\nHello">>, none),
    {ok, ProvResponse} = parse_datagram(<<"SIP/2.0 100 Trying\r\n\r\n">>),
    [?_assertEqual(true, is_request(Request)),
     ?_assertEqual(false, is_request(Response)),
     ?_assertEqual(false, is_response(Request)),
     ?_assertEqual(true, is_response(Response)),
     ?_assertEqual(true, is_provisional_response(ProvResponse)),
     ?_assertEqual(false, is_provisional_response(Response)),
     ?_assertEqual(<<"INVITE sip:urn:service:test SIP/2.0\r\nContent-Length: 5\r\nx-custom: Nothing\r\n\r\nHello">>, to_binary(Request)),
     ?_assertEqual(<<"SIP/2.0 200 Ok\r\nContent-Length: 5\r\n\r\nHello">>, to_binary(Response))
    ].


-spec create_ack_test_() -> list().
create_ack_test_() ->
    ReqHeaders = [
                  {'via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKkjshdyff">>},
                  {'to', <<"Bob <sip:bob@biloxi.com>">>},
                  {'from', <<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>},
                  {'call-id', <<"987asjd97y7atg">>},
                  {'cseq', <<"986759 INVITE">>},
                  {'route', <<"<sip:alice@atlanta.com>">>},
                  {'route', <<"<sip:bob@biloxi.com>">>}
                  ],
    OrigRequest = #sip_message{start_line = {request, 'INVITE', <<"sip:bob@biloxi.com">>}, headers = ReqHeaders},

    RespHeaders = lists:keyreplace('to', 1, ReqHeaders, {'to', <<"Bob <sip:bob@biloxi.com>;tag=1928301774">>}),
    Response = #sip_message{start_line = {response, 500, <<"Internal error">>}, headers = RespHeaders},

    ACKHeaders = lists:keyreplace('cseq', 1, RespHeaders, {'cseq', <<"986759 ACK">>}),
    ACK = #sip_message{start_line = {request, 'ACK', <<"sip:bob@biloxi.com">>}, headers = ACKHeaders},
    [
     ?_assertEqual(sort_headers(ACK), sort_headers(create_ack(OrigRequest, Response)))
     ].

-spec header_test_() -> list().
header_test_() ->

    CSeq = sip_headers:cseq(110, 'INVITE'),
    Via1 = sip_headers:via(udp, {<<"127.0.0.1">>, 5060}, [{branch, <<"z9hG4bK776asdhds">>}]),
    Via2 = sip_headers:via(tcp, {<<"127.0.0.2">>, 15060}, [{ttl, 4}]),
    Via1Up = sip_headers:via(udp, {<<"localhost">>, 5060}, []),
    Fun = fun (Value) when Value =:= Via1 -> Via1Up end,

    ViaMsg =  #sip_message{headers = [{'content-length', 123},
                                      {'via', [Via1]},
                                      {'via', [Via2]}]},
    ViaMsgUp = #sip_message{headers = [{'content-length', 123},
                                       {'via', [Via1Up]},
                                       {'via', [Via2]}]},
    NoViaMsg = #sip_message{headers = [{'content-length', 123}, {'cseq', CSeq}]},

    URI = <<"sip@nowhere.invalid">>,
    ValidRequest =
        #sip_message{start_line = {request, 'OPTIONS', URI},
                     headers = [{to, sip_headers:address(<<>>, URI, [])},
                                {from, sip_headers:address(<<>>, URI, [])},
                                {cseq, sip_headers:cseq(1, 'OPTIONS')},
                                {'call-id', <<"123">>},
                                {'max-forwards', 70},
                                {via, sip_headers:via(udp, <<"localhost">>, [])}]},

    % No contact in INVITE
    InvalidRequest =
        ValidRequest#sip_message{start_line = {request, 'INVITE', URI}},
    [% Header lookup functions
     ?_assertEqual({ok, Via1}, top_header('via', [{'via', [Via1, Via2]}])),
     ?_assertEqual({ok, <<"z9hG4bK776asdhds">>}, top_via_branch(#sip_message{headers = [{'via', [Via1]}, {'via', [Via2]}]})),
     ?_assertEqual({error, not_found}, top_via_branch(#sip_message{headers = [{'via', [Via2]}]})),

     % Header update functions
     ?_assertEqual(ViaMsgUp, update_top_header('via', Fun, ViaMsg)),
     ?_assertEqual(NoViaMsg, replace_top_header('via', Via1Up, NoViaMsg)),

     % Validation
     ?_assertEqual(ok, validate_request(ValidRequest)),
     ?_assertEqual({error, invalid_headers}, validate_request(InvalidRequest))
     ].
-endif.
