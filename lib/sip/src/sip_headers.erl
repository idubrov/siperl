%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% SIP headers parsing/generation.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_headers).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
-export([split_binary/1, to_binary/2]).
-export([parse_header/2, format_header/2]).
-export([via/3, cseq/2, address/3, add_tag/3]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("sip_common.hrl").
-include_lib("sip_parse.hrl").
-include_lib("sip.hrl").

%% Types
-type header_name() :: atom() | binary().
-type header() :: {HeaderName :: header_name(), Value :: term() | binary()}.
-type method() :: binary() | atom().
-type via_sent_by() :: {Host :: binary(), Port :: integer() | undefined}.
-type via() :: #sip_hdr_via{}.
-type address() :: #sip_hdr_address{}.
-type cseq() :: #sip_hdr_cseq{}.

-export_type([header_name/0, header/0, method/0, via_sent_by/0, via/0, address/0, cseq/0]).



%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc
%% Convert binary containing headers into list of non-parsed headers
%% (with binary values).
%% @end
-spec split_binary(binary()) -> [sip_headers:header()].
split_binary(Headers) when is_binary(Headers) ->
    Lines = binary:split(Headers, <<"\r\n">>, [global]),
    lists:reverse(lists:foldl(fun (Bin, List) -> fold_header(Bin, List) end, [], Lines)).

%% @doc
%% Convert header name and value into binary.
%% @end
-spec to_binary(header_name(), term()) -> binary().
to_binary(Name, Value) ->
    NameBin = header_name_to_binary(Name),
    ValueBin = format_header(Name, Value),
    <<NameBin/binary, ": ", ValueBin/binary>>.

%%-----------------------------------------------------------------
%% Header parsing/format functions
%%-----------------------------------------------------------------
-spec parse_header(HeaderName :: sip_headers:header_name(), Value :: any()) -> term().
parse_header(_Name, Header) when not is_binary(Header) ->
    % Already parsed
    Header;

%% Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
%% via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
parse_header('via', Bin) ->
    {{<<"SIP">>, Version, Transport}, Bin2} = parse_sent_protocol(Bin),
    {SentBy, Bin4} = parse_sent_by(Bin2),
    % Parse parameters (which should start with semicolon)
    {Params, Bin7} = parse_params(Bin4, []),

    Top = #sip_hdr_via{transport = Transport,
                       version = Version,
                       sent_by = SentBy,
                       params = Params},
    case Bin7 of
        <<?COMMA, Bin8/binary>> ->
            % Parse the rest of the Via
            % *(COMMA via-parm)
            case parse_header('via', Bin8) of
                Via when is_list(Via) -> [Top | Via];
                Via -> [Top | [Via]]
            end;
        _ -> Top
               end;

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
parse_header('content-length', Bin) ->
    sip_binary:binary_to_integer(Bin);

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
parse_header('cseq', Bin) ->
    {SeqBin, Bin2} = sip_binary:parse_token(Bin),
    {MethodBin, <<>>} = sip_binary:parse_token(Bin2),
    Sequence = sip_binary:binary_to_integer(SeqBin),
    Method = sip_binary:binary_to_existing_atom(sip_binary:to_upper(MethodBin)),
    #sip_hdr_cseq{sequence = Sequence, method = Method};

%% Max-Forwards  =  "Max-Forwards" HCOLON 1*DIGIT
parse_header('max-forwards', Bin) ->
    sip_binary:binary_to_integer(Bin);

%% Call-ID  =  ( "Call-ID" / "i" ) HCOLON callid
parse_header('call-id', Bin) ->
    Bin;

%% To             =  ( "To" / "t" ) HCOLON ( name-addr / addr-spec ) *( SEMI to-param )
%% to-param       =  tag-param / generic-param
%%
%% From           =  ( "From" / "f" ) HCOLON from-spec
%% from-spec      =  ( name-addr / addr-spec ) *( SEMI from-param )
%% from-param     =  tag-param / generic-param
%%
%% Contact        =  ("Contact" / "m" ) HCOLON
%%                  ( STAR / (contact-param *(COMMA contact-param)))
%% contact-param  =  (name-addr / addr-spec) *(SEMI contact-params)
%% contact-params     =  c-p-q / c-p-expires
%%                      / contact-extension
%% c-p-q              =  "q" EQUAL qvalue
%% c-p-expires        =  "expires" EQUAL delta-seconds
%% contact-extension  =  generic-param
%% delta-seconds      =  1*DIGIT
%%
%% name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
%% addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
%% display-name   =  *(token LWS)/ quoted-string
parse_header(Name, Bin) when Name =:= 'from'; Name =:= 'to'; Name =:= 'contact' ->
    {Display, URI, Params} = parse_fromto(sip_binary:trim_leading(Bin)),
    {Params2, Bin2} = parse_params(sip_binary:trim_leading(Params), []),
    Top = #sip_hdr_address{display_name = sip_binary:trim(Display), uri = URI, params = Params2},
    case Bin2 of
        <<?COMMA, Bin3/binary>> when Name =:= 'contact' ->
            % Parse the rest of the Contact
            % *(COMMA contact-param)
            case parse_header('contact', Bin3) of
                Contact when is_list(Contact) -> [Top | Contact];
                Contact -> [Top | [Contact]]
            end;
        <<>> -> Top
    end;



%% Any other header, just return value as is
parse_header(_Name, Value) ->
    Value.

%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
%% protocol-name     =  "SIP" / token
%% protocol-version  =  token
%% transport         =  "UDP" / "TCP" / "TLS" / "SCTP"
%%                      / other-transport
%% other-transport   =  token
parse_sent_protocol(Bin) ->
    {Protocol, <<$/, Bin2/binary>>} = sip_binary:parse_token(Bin),
    {Version, <<$/, Bin3/binary>>} = sip_binary:parse_token(Bin2),
    {Transport, Bin4} = sip_binary:parse_token(Bin3),
    Transport2 = sip_binary:binary_to_existing_atom(sip_binary:to_lower(Transport)),
    {{Protocol, Version, Transport2}, Bin4}.

%% sent-by           =  host [ COLON port ]
parse_sent_by(Bin) ->
    {Host, Bin2} = sip_binary:parse_token(Bin),
    {Port, Bin3} = case Bin2 of
                       <<?HCOLON, Rest/binary>>
                         ->
                           {P, Bin4} = sip_binary:parse_token(Rest),
                           {sip_binary:binary_to_integer(P), Bin4};
                       _ ->
                           {undefined, Bin2}
                   end,
    {{Host, Port}, Bin3}.

%% Parse parameters lists
%% *( SEMI param )
%% param  =  token [ EQUAL value ]
%% value  =  token / host / quoted-string
parse_params(<<?SEMI, Bin/binary>>, List) ->
    {Param, Rest} =
        case sip_binary:parse_token(Bin) of
            % Parameter with value
            {Name, <<?EQUAL, Bin2/binary>>} ->
                {Value, R} = sip_binary:parse_token_or_quoted_string(Bin2),
                {{sip_binary:binary_to_existing_atom(Name), Value}, R};

            % Parameter without a value
            {Name, Bin2} ->
                {sip_binary:binary_to_existing_atom(Name), Bin2}
        end,
    parse_params(Rest, [Param|List]);

parse_params(Bin, List) ->
    {lists:reverse(List), sip_binary:trim_leading(Bin)}.


%% @doc
%% Parse From/To header into display name, URI and binary with all the rest parameters
%% @end
parse_fromto(<<?DQUOTE, _/binary>> = Bin) ->
    % name-addr with quoted-string display-name
    {Display, <<?LAQUOT, Rest/binary>>} = sip_binary:parse_quoted_string(Bin),
    {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {Display, URI, Params};
parse_fromto(<<?LAQUOT, Rest/binary>>) ->
    % name-addr without display-name
    {URI, <<?RAQUOT, Rest2/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {<<>>, URI, Rest2};
parse_fromto(Bin) ->
    % either addr-spec or name-addr with token-based display-name
    case sip_binary:parse_until(Bin, ?LAQUOT) of
        {_Any, <<>>} ->
            % addr-spec
            {URI, Params} = sip_binary:parse_until(Bin, fun sip_binary:is_space_char/1),
            {<<>>, URI, Params};

        {Display, <<?LAQUOT, Rest/binary>>} ->
            % name-addr with token-based display-name
            {URI, <<?RAQUOT, Rest2/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
            {Display, URI, Rest2}
    end.

%% @doc
%% Format header into the binary.
%% @end
-spec format_header(header_name(), term()) -> binary().
format_header(_Name, Value) when is_binary(Value) ->
    % already formatted to binary
    Value;
% generalized multi-value headers handling
format_header(Name, [Value]) -> format_header(Name, Value);
format_header(Name, [Top | Rest]) ->
    Joiner = fun (Elem, Bin) ->
                      ElemBin = format_header(Name, Elem),
                      <<Bin/binary, ?COMMA, ElemBin/binary>>
             end,
    TopBin = format_header(Name, Top),
    lists:foldl(Joiner, TopBin, Rest);
%% Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
%% via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
format_header('via', Via) when is_record(Via, sip_hdr_via) ->
    Version = Via#sip_hdr_via.version,
    Transport = sip_binary:to_upper(sip_binary:any_to_binary(Via#sip_hdr_via.transport)),
    Bin = <<"SIP/", Version/binary, $/, Transport/binary>>,
    Bin2 = case Via#sip_hdr_via.sent_by of
        {Host, undefined} ->
            <<Bin/binary, ?SP, Host/binary>>;

        {Host, Port} ->
            <<Bin/binary, ?SP, Host/binary, ?HCOLON, (sip_binary:integer_to_binary(Port))/binary>>
    end,
    append_params(Bin2, Via#sip_hdr_via.params);

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
format_header('content-length', Length) when is_integer(Length) ->
    sip_binary:integer_to_binary(Length);

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
format_header('cseq', CSeq) when is_record(CSeq, sip_hdr_cseq) ->
    SequenceBin = sip_binary:integer_to_binary(CSeq#sip_hdr_cseq.sequence),
    MethodBin = sip_binary:any_to_binary(CSeq#sip_hdr_cseq.method),
    <<SequenceBin/binary, " ", MethodBin/binary>>;

%% Max-Forwards  =  "Max-Forwards" HCOLON 1*DIGIT
format_header('max-forwards', Hops) when is_integer(Hops) ->
    sip_binary:integer_to_binary(Hops);

%% Call-ID  =  ( "Call-ID" / "i" ) HCOLON callid
%% Call id is always a binary, so handled by first case

%% To             =  ( "To" / "t" ) HCOLON ( name-addr / addr-spec ) *( SEMI to-param )
%% to-param       =  tag-param / generic-param
%%
%% From           =  ( "From" / "f" ) HCOLON from-spec
%% from-spec      =  ( name-addr / addr-spec ) *( SEMI from-param )
%% from-param     =  tag-param / generic-param
%%
%% Contact        =  ("Contact" / "m" ) HCOLON
%%                  ( STAR / (contact-param *(COMMA contact-param)))
%% contact-param  =  (name-addr / addr-spec) *(SEMI contact-params)
%% contact-params     =  c-p-q / c-p-expires
%%                      / contact-extension
%% c-p-q              =  "q" EQUAL qvalue
%% c-p-expires        =  "expires" EQUAL delta-seconds
%% contact-extension  =  generic-param
%% delta-seconds      =  1*DIGIT
%%
%% name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
%% addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
%% display-name   =  *(token LWS)/ quoted-string
format_header(Name, Addr) when
  is_record(Addr, sip_hdr_address), (Name =:= 'from' orelse Name =:= 'to' orelse Name =:= 'contact') ->

    URI = Addr#sip_hdr_address.uri,
    Bin = case Addr#sip_hdr_address.display_name of
              <<>> -> <<?LAQUOT, URI/binary, ?RAQUOT>>;
              DisplayName -> <<DisplayName/binary, " ", ?LAQUOT, URI/binary, ?RAQUOT>>
          end,
    append_params(Bin, Addr#sip_hdr_address.params);

%% Any other header
format_header(_Name, Value) ->
    sip_binary:any_to_binary(Value).

%% @doc
%% Append parameters to the binary
%% @end
append_params(Bin, Params) ->
    lists:foldl(fun format_param/2, Bin, Params).

%% @doc
%% Format semi-colon separated list of parameters
%% Each parameter is either binary (parameter name) or
%% tuple of two binaries (parameter name and value).
%% @end
format_param({Name, Value}, Bin) ->
    Name2 = sip_binary:any_to_binary(Name),
    Value2 = sip_binary:any_to_binary(Value),
    <<Bin/binary, ?SEMI, Name2/binary, ?EQUAL, Value2/binary>>;

format_param(Name, Bin) ->
    Name2 = sip_binary:any_to_binary(Name),
    <<Bin/binary, ?SEMI, Name2/binary>>.

%%-----------------------------------------------------------------
%% Header-specific helpers
%%-----------------------------------------------------------------
%% @doc
%% Construct Via header value.
%% @end
-spec via(atom(), via_sent_by() | binary(), [any()]) -> #sip_hdr_via{}.
via(Transport, {Host, Port}, Params) when
  is_atom(Transport), is_list(Params) ->
    #sip_hdr_via{transport = Transport, sent_by = {Host, Port}, params = Params};
via(Transport, Host, Params) ->
    via(Transport, {Host, 5060}, Params).

%% @doc
%% Construct CSeq header value.
%% @end
-spec cseq(integer(), method()) -> #sip_hdr_cseq{}.
cseq(Sequence, Method) when
  is_integer(Sequence),
  (is_atom(Method) orelse is_binary(Method)) ->
    #sip_hdr_cseq{method = Method, sequence = Sequence}.


%% @doc
%% Construct address (value of From/To headers).
%% @end
-spec address(binary(), binary(), list()) -> #sip_hdr_address{}.
address(DisplayName, URI, Params) when
  is_binary(DisplayName),
  is_binary(URI),
  is_list(Params) ->
    #sip_hdr_address{display_name = DisplayName, uri = URI, params = Params}.

%% @doc Add tag to the `From:' or `To:` header.
%% @end
-spec add_tag(atom(), #sip_hdr_address{}, binary() | undefined) -> #sip_hdr_address{}.
add_tag(Name, Value, undefined) when Name =:= 'to'; Name =:= 'from' -> Value;
add_tag(Name, Value, Tag) when Name =:= 'to'; Name =:= 'from' ->
    Value2 = sip_headers:parse_header(Name, Value),
    Params = lists:keystore('tag', 1, Value2#sip_hdr_address.params, {'tag', Tag}),
    Value2#sip_hdr_address{params = Params}.



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

% RFC 3261, 7.3.1
% The line break and the whitespace at the beginning of the next
% line are treated as a single SP character. This function appends
% such lines to the last header.
fold_header(<<C/utf8, _/binary>> = Line, [{Name, Value} | Tail]) when
  C =:= ?SP; C =:= ?HTAB ->
    Line2 = sip_binary:trim_leading(Line),
    Value2 = sip_binary:trim_trailing(Value),
    [{Name, <<Value2/binary, ?SP, Line2/binary>>} | Tail];

fold_header(HeaderLine, List) ->
    [Name, Value] = binary:split(HeaderLine, <<?HCOLON>>),
    Name2 = sip_binary:to_lower(sip_binary:trim_trailing(Name)),
    Name3 = binary_to_header_name(Name2),
    [{Name3, sip_binary:trim_leading(Value)} | List].

%% Converting binary to header name
%% Short header names support
binary_to_header_name(<<"v">>) -> 'via';
binary_to_header_name(<<"l">>) -> 'content-length';
binary_to_header_name(<<"f">>) -> 'from';
binary_to_header_name(<<"t">>) -> 'to';
binary_to_header_name(<<"i">>) -> 'call-id';
binary_to_header_name(<<"m">>) -> 'contact';
binary_to_header_name(Bin) -> sip_binary:binary_to_existing_atom(Bin).

%% Converting header name back to binary
header_name_to_binary('via') -> <<"Via">>;
header_name_to_binary('content-length') -> <<"Content-Length">>;
header_name_to_binary('cseq') -> <<"CSeq">>;
header_name_to_binary('max-forwards') -> <<"Max-Forwards">>;
header_name_to_binary('call-id') -> <<"Call-Id">>;
header_name_to_binary('from') -> <<"From">>;
header_name_to_binary('to') -> <<"To">>;
header_name_to_binary('contact') -> <<"Contact">>;
header_name_to_binary(Name) -> sip_binary:any_to_binary(Name).

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec parse_test_() -> list().
parse_test_() ->
    [
     ?_assertEqual([{'content-length', <<"5">>}],
                   split_binary(<<"Content-Length: 5">>)),
     ?_assertEqual([{'content-length', <<"5">>}, {'via', <<"SIP/2.0/UDP localhost">>},
                    {'from', <<"sip:alice@localhost">>}, {'to', <<"sip:bob@localhost">>},
                    {'call-id', <<"callid">>}],
                   split_binary(<<"l: 5\r\nv: SIP/2.0/UDP localhost\r\nf: sip:alice@localhost\r\nt: sip:bob@localhost\r\ni: callid">>)),
     ?_assertEqual([{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}],
                   split_binary(<<"X-Custom: Nothing\r\nContent-Length: 5">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   split_binary(<<"Subject: I know you're there,\r\n               pick up the phone   \r\n               and talk to me!">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   split_binary(<<"Subject: I know you're there,\r\n\tpick up the phone    \r\n               and talk to me!">>)),

     % Already parsed
     ?_assertEqual({parsed, value}, parse_header('x-custom2', {parsed, value})),

     % Custom header
     ?_assertEqual(<<"custom">>, parse_header('x-custom2', <<"custom">>)),

     % Content-Length
     ?_assertEqual(32543523, parse_header('content-length', <<"32543523">>)),
     ?_assertEqual(<<"98083">>,
                   format_header('content-length', 98083)),
     % CSeq
     ?_assertEqual(#sip_hdr_cseq{sequence = 1231, method = 'ACK'},
                   parse_header('cseq', <<"1231 ACK">>)),
     ?_assertEqual(<<"123453 INVITE">>,
                   format_header('cseq', #sip_hdr_cseq{sequence=  123453, method = 'INVITE'})),
     ?_assertEqual(<<"123453 INVITE">>,
                   format_header('cseq', <<"123453 INVITE">>)),

     % Max-Forwards
     ?_assertEqual(70, parse_header('max-forwards', <<"70">>)),
     ?_assertEqual(<<"70">>,
                   format_header('max-forwards', 70)),

     % From/To
     ?_assertEqual(address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"Bob  Zert <sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"sip:bob@biloxi.com ;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"<sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),

     ?_assertEqual(<<"Bob  Zert <sip:bob@biloxi.com>;tag=1928301774">>,
                   format_header('from', address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format_header('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format_header('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format_header('from', address(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format_header('to', address(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),

     % Contact
     ?_assertEqual(address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                   parse_header('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1">>)),
     ?_assertEqual([address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                    address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, <<"0.2">>}])],
                   parse_header('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1,Alice <sip:alice@atlanta.com>;q=0.2">>)),

     ?_assertEqual(<<"Bob <sip:bob@biloxi.com>;q=0.1">>,
                   format_header('contact', address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]))),
     ?_assertEqual(<<"Bob <sip:bob@biloxi.com>;q=0.1,Alice <sip:alice@atlanta.com>;q=0.2">>,
                   format_header('contact', [address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                                             address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, <<"0.2">>}])])),

     % Via
     ?_assertEqual(via(udp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                   parse_header('via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse_header('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                    via(udp, {<<"pc22.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK43nthoeu3">>}])],
                   parse_header('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds,SIP/2.0/UDP pc22.atlanta.com;branch=z9hG4bK43nthoeu3">>)),
     ?_assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,
                   format_header('via', [via(udp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com:5060;branch=z9hG4bK776asdhds">>,
                   format_header('via', [via(udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]),
                                         via(tcp, <<"pc33.atlanta.com">>, [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>,
                   format_header('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>)),

     % Formatting
     ?_assertEqual(<<"I know you're there, pick up the phone and talk to me!">>,
        format_header('subject', <<"I know you're there, pick up the phone and talk to me!">>))
    ].

-endif.
