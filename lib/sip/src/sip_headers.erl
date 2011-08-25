%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP headers parsing/generation and utility functions
%%%
%%% FIXME: need to verify that all binary generation properly unescapes/escapes characters!
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_headers).

%%-----------------------------------------------------------------
%% Exports
%%-----------------------------------------------------------------
-export([parse_headers/1, format_headers/1]).
-export([parse/2, format/2]).
-export([via/3, cseq/2, address/3]).
-export([add_tag/3, qvalue/1]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include("sip_common.hrl").
-include("sip_parse.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc Parse binary into list of headers
%%
%% Convert binary containing headers into list of non-parsed headers
%% (with binary values). Binary must be `\r\n' separated headers. If
%% at least one header is present, binary must end with `\r\n'. Otherwise,
%% it must be empty binary.
%% @end
-spec parse_headers(binary()) -> [{Name :: atom() | binary(), Value :: binary() | term()}].
parse_headers(<<>>) -> [];
parse_headers(Headers) when is_binary(Headers) ->
    Pos = size(Headers) - 2,
    <<Headers2:Pos/binary, "\r\n">> = Headers,
    Lines = binary:split(Headers2, <<"\r\n">>, [global]),
    lists:reverse(lists:foldl(fun (Bin, List) -> fold_header(Bin, List) end, [], Lines)).

%% @doc Convert header name and value into the binary
%% @end
-spec format_headers([{atom() | binary(), binary() | term()}]) -> binary().
format_headers(Headers) ->
    << <<(header_name_to_binary(Name))/binary, ": ",
         (format(Name, Value))/binary, "\r\n">> ||
       {Name, Value} <- Headers>>.

%%-----------------------------------------------------------------
%% Header parsing/format functions
%%-----------------------------------------------------------------

%% @doc Parse header binary value into term representation
%% @end
-spec parse(Name :: atom() | binary(), Value :: any()) -> term().
parse(_Name, Header) when not is_binary(Header) ->
    % Already parsed
    Header;
%% Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
%% via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
parse('via', Bin) ->
    {{<<"SIP">>, Version, Transport}, Bin2} = parse_sent_protocol(Bin),
    % Parse parameters (which should start with semicolon)
    {Host, Port, Bin3} = sip_binary:parse_host_port(Bin2),
    {Params, Bin4} = parse_params(Bin3, fun parse_via_param/2),

    Top = #sip_hdr_via{transport = Transport,
                       version = Version,
                       host = Host,
                       port = Port,
                       params = Params},
    case Bin4 of
        <<?COMMA, Bin5/binary>> ->
            % Parse the rest of the Via
            % *(COMMA via-parm)
            case parse('via', Bin5) of
                Via when is_list(Via) -> [Top | Via];
                Via -> [Top | [Via]]
            end;
        <<>> -> Top
               end;

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
parse('content-length', Bin) ->
    sip_binary:binary_to_integer(Bin);

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
parse('cseq', Bin) ->
    {SeqBin, Bin2} = sip_binary:parse_token(Bin),
    {MethodBin, <<>>} = sip_binary:parse_token(Bin2),
    Sequence = sip_binary:binary_to_integer(SeqBin),
    Method = sip_binary:binary_to_existing_atom(sip_binary:to_upper(MethodBin)),
    #sip_hdr_cseq{sequence = Sequence, method = Method};

%% Max-Forwards  =  "Max-Forwards" HCOLON 1*DIGIT
parse('max-forwards', Bin) ->
    sip_binary:binary_to_integer(Bin);

%% Call-ID  =  ( "Call-ID" / "i" ) HCOLON callid
%% callid   =  word [ "@" word ]
%% word     =  1*(alphanum / "-" / "." / "!" / "%" / "*" /
%%             "_" / "+" / "`" / "'" / "~" /
%%             "(" / ")" / "<" / ">" /
%%             ":" / "\" / DQUOTE /
%%             "/" / "[" / "]" / "?" /
%%             "{" / "}" )
parse('call-id', Bin) -> Bin;

%% To             =  ( "To" / "t" ) HCOLON ( name-addr / addr-spec ) *( SEMI to-param )
%% to-param       =  tag-param / generic-param
%%
%% From           =  ( "From" / "f" ) HCOLON from-spec
%% from-spec      =  ( name-addr / addr-spec ) *( SEMI from-param )
%% from-param     =  tag-param / generic-param
%%
%% name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
%% addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
%% display-name   =  *(token LWS)/ quoted-string
parse(Name, Bin) when Name =:= 'from'; Name =:= 'to' ->
    {Top, <<>>} = parse_address(Bin),
    Top;
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
%% Route              =  "Route" HCOLON route-param *(COMMA route-param)
%% route-param        =  name-addr *( SEMI rr-param )
%% rr-param           =  generic-param
%%
%% Record-Route       =  "Record-Route" HCOLON rec-route *(COMMA rec-route)
%% rec-route          =  name-addr *( SEMI rr-param )
%%
%% name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
%% addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
%% display-name   =  *(token LWS)/ quoted-string
parse('contact', <<"*">>) -> '*';
parse(Name, Bin) when Name =:= 'contact'; Name =:= 'route'; Name =:= 'record-route' ->
    case parse_address(Bin) of
        {Top, <<?COMMA, Bin2/binary>>} ->
            % Parse the rest of the Contact, Route or Record-Route headers
            % *(COMMA contact-param / route-param / rec-route)
            case parse(Name, Bin2) of
                Value when is_list(Value) -> [Top | Value];
                Value -> [Top | [Value]]
            end;
        {Top, <<>>} -> Top
    end;
%% Any other header, just return value as is
parse(_Name, Value) ->
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

%% Parse parameters lists
%% *( SEMI param )
%% param  =  token [ EQUAL value ]
%% value  =  token / host / quoted-string
parse_params(Bin, ParseFun) ->
    parse_params_loop(sip_binary:trim_leading(Bin), ParseFun, []).

parse_params_loop(<<?SEMI, Bin/binary>>, ParseFun, List) ->
    {NameBin, MaybeValue} = sip_binary:parse_token(Bin),
    Name = sip_binary:binary_to_existing_atom(NameBin),
    {Value, Rest} =
        case MaybeValue of
            % Parameter with value
            <<?EQUAL, Bin2/binary>> ->
                parse_token_or_quoted(Bin2);
            % Parameter without a value ('true' value)
            Next ->
                {true, Next}
        end,
    ParsedValue = ParseFun(Name, Value),
    Property = proplists:property(Name, ParsedValue),
    parse_params_loop(Rest, ParseFun, [Property | List]);
parse_params_loop(Bin, _ParseFun, List) ->
    {lists:reverse(List), sip_binary:trim_leading(Bin)}.

parse_token_or_quoted(Bin) ->
    case sip_binary:trim_leading(Bin) of
        <<?DQUOTE, _Rest/binary>> -> sip_binary:parse_quoted_string(Bin);
        _Token -> sip_binary:parse_token(Bin)
    end.

%% @doc Parse address-like header into display name, URI and binary with the parameters
%%
%% Parses values like
%% ```
%% value       = ( name-addr / addr-spec )
%% '''
%% @end
parse_address_uri(<<?DQUOTE, _/binary>> = Bin) ->
    % name-addr with quoted-string display-name
    {Display, <<?LAQUOT, Rest/binary>>} = sip_binary:parse_quoted_string(Bin),
    {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {Display, URI, Params};
parse_address_uri(<<?LAQUOT, Rest/binary>>) ->
    % name-addr without display-name
    {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {<<>>, URI, Params};
parse_address_uri(Bin) ->
    % either addr-spec or name-addr with token-based display-name
    case sip_binary:parse_until(Bin, ?LAQUOT) of
        {_Any, <<>>} ->
            % addr-spec
            {URI, Params} = sip_binary:parse_until(Bin, fun sip_binary:is_space_char/1),
            {<<>>, URI, Params};

        {Display, <<?LAQUOT, Rest/binary>>} ->
            % name-addr with token-based display-name
            {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
            {Display, URI, Params}
    end.

%% @doc Parse address-like header together with all parameters
%%
%% Parses values like
%% ```
%% value       = ( name-addr / addr-spec ) *( SEMI param )
%% param       =  tag-param / generic-param
%% '''
%% @end
parse_address(Bin) ->
    {Display, URI, Bin2} = parse_address_uri(sip_binary:trim_leading(Bin)),
    {Params, Bin3} = parse_params(Bin2, fun (_Name, Value) -> Value end),
    Value = #sip_hdr_address{display_name = sip_binary:trim(Display),
                             uri = sip_uri:parse(URI),
                             params = Params},
    {Value, Bin3}.

%% @doc Format header value into the binary.
%% @end
-spec format(atom() | binary(), binary() | term()) -> binary().
format(_Name, Value) when is_binary(Value) ->
    % already formatted to binary
    Value;
% generalized multi-value headers handling
format(Name, [Value]) -> format(Name, Value);
format(Name, [Top | Rest]) ->
    Joiner = fun (Elem, Bin) ->
                      ElemBin = format(Name, Elem),
                      <<Bin/binary, ?COMMA, ElemBin/binary>>
             end,
    TopBin = format(Name, Top),
    lists:foldl(Joiner, TopBin, Rest);
%% Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
%% via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
format('via', Via) when is_record(Via, sip_hdr_via) ->
    Version = Via#sip_hdr_via.version,
    Transport = sip_binary:to_upper(atom_to_binary(Via#sip_hdr_via.transport, latin1)),
    Bin = <<"SIP/", Version/binary, $/, Transport/binary>>,
    Host = sip_binary:addr_to_binary(Via#sip_hdr_via.host),
    Bin2 = case Via#sip_hdr_via.port of
        undefined ->
            <<Bin/binary, ?SP, Host/binary>>;
        Port ->
            <<Bin/binary, ?SP, Host/binary, ?HCOLON, (sip_binary:integer_to_binary(Port))/binary>>
    end,
    append_params(Bin2, Via#sip_hdr_via.params);

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
format('content-length', Length) when is_integer(Length) ->
    sip_binary:integer_to_binary(Length);

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
format('cseq', CSeq) when is_record(CSeq, sip_hdr_cseq) ->
    SequenceBin = sip_binary:integer_to_binary(CSeq#sip_hdr_cseq.sequence),
    MethodBin = sip_binary:any_to_binary(CSeq#sip_hdr_cseq.method),
    <<SequenceBin/binary, " ", MethodBin/binary>>;

%% Max-Forwards  =  "Max-Forwards" HCOLON 1*DIGIT
format('max-forwards', Hops) when is_integer(Hops) ->
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
%% Route              =  "Route" HCOLON route-param *(COMMA route-param)
%% route-param        =  name-addr *( SEMI rr-param )
%% rr-param           =  generic-param
%%
%% Record-Route       =  "Record-Route" HCOLON rec-route *(COMMA rec-route)
%% rec-route          =  name-addr *( SEMI rr-param )
%%
%% name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
%% addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
%% display-name   =  *(token LWS)/ quoted-string
format('contact', '*') -> <<"*">>;
format(Name, #sip_hdr_address{} = Addr) when
  Name =:= 'from';
  Name =:= 'to';
  Name =:= 'contact';
  Name =:= 'route';
  Name =:= 'record-route' ->

    URIBin = sip_uri:format(Addr#sip_hdr_address.uri),
    Bin = case Addr#sip_hdr_address.display_name of
              <<>> -> <<?LAQUOT, URIBin/binary, ?RAQUOT>>;
              DisplayName ->
                  Quoted = sip_binary:quote_string(DisplayName),
                  <<Quoted/binary, " ", ?LAQUOT, URIBin/binary, ?RAQUOT>>
          end,
    append_params(Bin, Addr#sip_hdr_address.params);

%% Any other header
format(_Name, Value) ->
    sip_binary:any_to_binary(Value).

%% @doc Append parameters to the binary
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

    % If contains non-token characters, write as quoted string
    Value2 = case need_quoting(Value) of
                 true -> sip_binary:quote_string(Value);
                 false -> Value
             end,
    Value3 = sip_binary:any_to_binary(Value2),
    <<Bin/binary, ?SEMI, Name2/binary, ?EQUAL, Value3/binary>>;
format_param(Name, Bin) ->
    Name2 = sip_binary:any_to_binary(Name),
    <<Bin/binary, ?SEMI, Name2/binary>>.

need_quoting(Value) when not is_binary(Value) ->
    % no need to escape non-binary values
    % (it could be number, IP address, atom)
    false;
need_quoting(<<>>) ->
    false;
need_quoting(<<C, Rest/binary>>)  ->
    (not sip_binary:is_token_char(C)) orelse need_quoting(Rest).

%%-----------------------------------------------------------------
%% Header-specific helpers
%%-----------------------------------------------------------------
%% @doc
%% Construct Via header value.
%% @end
-spec via(atom(), {string() | inet:ip_address(), integer() | 'undefined'} | string(), [any()]) -> #sip_hdr_via{}.
via(Transport, {Host, Port}, Params) when
  is_atom(Transport), is_list(Params), (is_list(Host) orelse is_tuple(Host)) ->
    #sip_hdr_via{transport = Transport, host = Host, port = Port, params = Params};
via(Transport, Host, Params) when is_list(Host); is_tuple(Host) ->
    via(Transport, {Host, 5060}, Params).

%% @doc
%% Construct CSeq header value.
%% @end
-spec cseq(integer(), atom() | binary()) -> #sip_hdr_cseq{}.
cseq(Sequence, Method) when
  is_integer(Sequence),
  (is_atom(Method) orelse is_binary(Method)) ->
    #sip_hdr_cseq{method = Method, sequence = Sequence}.


%% @doc Construct address (value of From/To headers).
%%
%% <em>Note: parses URI if it is given in binary form</em>
%% @end
-spec address(binary(), #sip_uri{} | #tel_uri{} | binary(), list()) -> #sip_hdr_address{}.
address(DisplayName, URI, Params) when is_binary(DisplayName), is_list(Params), is_binary(URI) ->
    #sip_hdr_address{display_name = DisplayName, uri = sip_uri:parse(URI), params = Params};
address(DisplayName, URI, Params) when is_binary(DisplayName), is_list(Params) ->
    #sip_hdr_address{display_name = DisplayName, uri = URI, params = Params}.

%%% @doc Return value of the `q' parameter of the address as floating point number.
%%% @end
-spec qvalue(#sip_hdr_address{}) -> float().
qvalue(Address) when is_record(Address, sip_hdr_address) ->
    case lists:keyfind('q', 1, Address#sip_hdr_address.params) of
        false ->
            1.0; % default q-value
        {'q', Value} when is_float(Value) ->
            Value;
        {'q', Bin} when is_binary(Bin) ->
            sip_binary:binary_to_float(Bin)
    end.

%% @doc Add tag to the `From:' or `To:' header.
%% @end
-spec add_tag(atom(), #sip_hdr_address{}, binary() | undefined) -> #sip_hdr_address{}.
add_tag(Name, Value, undefined) when Name =:= 'to'; Name =:= 'from' -> Value;
add_tag(Name, Value, Tag) when Name =:= 'to'; Name =:= 'from' ->
    Value2 = parse(Name, Value),
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
header_name_to_binary(Name) when is_binary(Name) -> Name;
header_name_to_binary(Name) when is_atom(Name) -> atom_to_binary(Name, utf8).

%% Parse standard Via: parameters
parse_via_param('ttl', TTL) -> sip_binary:binary_to_integer(TTL);
parse_via_param('maddr', MAddr) ->
    case sip_binary:parse_ip_address(MAddr) of
        {ok, Addr} -> Addr;
        {error, einval} -> binary_to_list(MAddr)
    end;
parse_via_param('received', Received) ->
    {ok, Addr} = sip_binary:parse_ip_address(Received),
    Addr;
parse_via_param(_Name, Value) -> Value.

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec parse_test_() -> list().
parse_test_() ->
    [
     % Parsing
     ?_assertEqual([{'content-length', <<"5">>}],
                   parse_headers(<<"Content-Length: 5\r\n">>)),
     ?_assertEqual([{'content-length', <<"5">>}, {'via', <<"SIP/2.0/UDP localhost">>},
                    {'from', <<"sip:alice@localhost">>}, {'to', <<"sip:bob@localhost">>},
                    {'call-id', <<"callid">>}],
                   parse_headers(<<"l: 5\r\nv: SIP/2.0/UDP localhost\r\nf: sip:alice@localhost\r\nt: sip:bob@localhost\r\ni: callid\r\n">>)),
     ?_assertEqual([{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}],
                   parse_headers(<<"X-Custom: Nothing\r\nContent-Length: 5\r\n">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   parse_headers(<<"Subject: I know you're there,\r\n               pick up the phone   \r\n               and talk to me!\r\n">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   parse_headers(<<"Subject: I know you're there,\r\n\tpick up the phone    \r\n               and talk to me!\r\n">>)),

     % Formatting
     ?_assertEqual(<<"Content-Length: 5\r\n">>,
                   format_headers([{'content-length', <<"5">>}])),
     ?_assertEqual(<<"x-custom: Nothing\r\nContent-Length: 5\r\n">>,
                   format_headers([{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}])),

     % Already parsed
     ?_assertEqual({parsed, value}, parse('x-custom2', {parsed, value})),

     % Custom header
     ?_assertEqual(<<"custom">>, parse('x-custom2', <<"custom">>)),

     % Content-Length
     ?_assertEqual(32543523, parse('content-length', <<"32543523">>)),
     ?_assertEqual(<<"98083">>,
                   format('content-length', 98083)),
     % CSeq
     ?_assertEqual(#sip_hdr_cseq{sequence = 1231, method = 'ACK'},
                   parse('cseq', <<"1231 ACK">>)),
     ?_assertEqual(<<"123453 INVITE">>,
                   format('cseq', #sip_hdr_cseq{sequence=  123453, method = 'INVITE'})),
     ?_assertEqual(<<"123453 INVITE">>,
                   format('cseq', <<"123453 INVITE">>)),

     % Max-Forwards
     ?_assertEqual(70, parse('max-forwards', <<"70">>)),
     ?_assertEqual(<<"70">>,
                   format('max-forwards', 70)),

     % From/To
     ?_assertEqual(address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"Bob  Zert <sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"sip:bob@biloxi.com ;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"<sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),

     ?_assertEqual(<<"\"Bob  Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('to', address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob \\\"Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('to', address(<<"Bob \"Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),

     % Contact
     ?_assertEqual(address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                   parse('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1">>)),
     ?_assertEqual([address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                    address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, <<"0.2">>}])],
                   parse('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1,\"Alice\" <sip:alice@atlanta.com>;q=0.2">>)),
     ?_assertEqual('*', parse('contact', <<"*">>)),

     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;q=0.1">>,
                   format('contact', address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]))),
     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;q=0.1,\"Alice\" <sip:alice@atlanta.com>;q=0.2">>,
                   format('contact', [address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, <<"0.1">>}]),
                                             address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, <<"0.2">>}])])),
     ?_assertEqual(<<"*">>, format('contact', '*')),

     % q-value
     ?_assertEqual(1.0, qvalue(address(<<"Alice">>, <<"sip:alice@atlanta.com">>, []))),
     ?_assertEqual(0.2, qvalue(address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, <<"0.2">>}]))),
     ?_assertEqual(0.5, qvalue(address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, 0.5}]))),

     % Route, Record-Route
     ?_assertEqual(address(<<>>, <<"sip:p1.example.com;lr">>, []),
                   parse('route', <<"<sip:p1.example.com;lr>">>)),
     ?_assertEqual(address(<<>>, <<"sip:p1.example.com;lr">>, []),
                   parse('record-route', <<"<sip:p1.example.com;lr>">>)),

     ?_assertEqual(<<"<sip:p1.example.com;lr>">>,
                   format('route', address(<<>>, <<"sip:p1.example.com;lr">>, []))),
     ?_assertEqual(<<"<sip:p1.example.com;lr>">>,
                   format('record-route', address(<<>>, <<"sip:p1.example.com;lr">>, []))),

     % Via
     ?_assertEqual(via(udp, {{8193,3512,0,0,0,0,44577,44306}, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                   parse('via', <<"SIP/2.0/UDP [2001:0db8:0000:0000:0000:0000:ae21:ad12];branch=z9hG4bK776asdhds">>)),
     ?_assertEqual(via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual(via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com ; branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                    via(udp, {"pc22.atlanta.com", undefined}, [{branch, <<"z9hG4bK43nthoeu3">>}])],
                   parse('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds,SIP/2.0/UDP pc22.atlanta.com;branch=z9hG4bK43nthoeu3">>)),
     ?_assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,
                   format('via', [via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com:5060;branch=z9hG4bK776asdhds">>,
                   format('via', [via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                                         via(tcp, "pc33.atlanta.com", [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>,
                   format('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>)),
     ?_assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;extra=\"Hello world\"">>,
                   format('via', [via(udp, {"pc33.atlanta.com", undefined}, [{extra, <<"Hello world">>}])])),
     ?_assertEqual(via(udp, {"pc33.atlanta.com", undefined}, [{ttl, 3}, {maddr, {224, 0, 0, 1}}, {received, {127, 0, 0, 1}}, {branch, <<"z9hG4bK776asdhds">>}]),
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;ttl=3;maddr=224.0.0.1;received=127.0.0.1;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual(via(udp, {"pc33.atlanta.com", undefined}, [{ttl, 3}, {maddr, "sip.mcast.net"}, {received, {127, 0, 0, 1}}, {branch, <<"z9hG4bK776asdhds">>}]),
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;ttl=3;maddr=sip.mcast.net;received=127.0.0.1;branch=z9hG4bK776asdhds">>)),

     % Formatting
     ?_assertEqual(<<"I know you're there, pick up the phone and talk to me!">>,
        format('subject', <<"I know you're there, pick up the phone and talk to me!">>))
    ].

-endif.
