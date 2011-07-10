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
-export([split_binary/1, update_top_header/3]).
-export([parse_header/2, format_header/1]).
-export([top_via/1, top_via_branch/1]).
-export([via/3, via/1, cseq/2, content_length/1, from/3, to/3]).

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

-export_type([header_name/0, header/0, method/0, via_sent_by/0]).



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
%% Update value of the top header with given name
%% @end
-spec update_top_header(HeaderName :: sip_headers:header_name(),
                        fun((HeaderName :: sip_headers:header_name(), Value :: any()) -> UpdatedValue :: any()),
                        [sip_headers:header()]) -> [sip_headers:header()].
update_top_header(HeaderName, Fun, List) ->
    update_top_header(HeaderName, Fun, [], List).

update_top_header(HeaderName, Fun, Unmatched, [{HeaderName, Value} | Tail]) ->
    Value2 = Fun(HeaderName, Value),
    lists:reverse([{HeaderName, Value2} | Unmatched]) ++ Tail;

update_top_header(HeaderName, Fun, Unmatched, [H | Tail]) ->
    update_top_header(HeaderName, Fun, [H|Unmatched], Tail);

update_top_header(_HeaderName, _Fun, Unmatched, []) ->
    % TODO: Nothing is matched -- we probably can return the original list.
    lists:reverse(Unmatched).

%%-----------------------------------------------------------------
%% Header parsing/format functions
%%-----------------------------------------------------------------
-spec parse_header(HeaderName :: sip_headers:header_name(), Value :: any()) -> sip_headers:header().
parse_header(Name, Header) when not is_binary(Header) ->
    % Already parsed
    {Name, Header};

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

    % Parse the rest of the Via
    % *(COMMA via-parm)
    RestHdrs = case Bin7 of
                   <<?COMMA, Bin8/binary>> ->
                       {'via', Via} = parse_header('via', Bin8),
                       Via;
                   _ ->
                       []
               end,
    {'via', [#sip_hdr_via{transport = Transport,
                          version = Version,
                          sent_by = SentBy,
                          params = Params} | RestHdrs]};

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
parse_header('content-length', Bin) ->
    {'content-length', sip_binary:to_integer(Bin)};

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
parse_header('cseq', Bin) ->
    {SeqBin, Bin2} = sip_binary:parse_token(Bin),
    {MethodBin, <<>>} = sip_binary:parse_token(Bin2),
    Sequence = sip_binary:to_integer(SeqBin),
    Method = sip_binary:try_binary_to_existing_atom(sip_binary:to_upper(MethodBin)),
    {'cseq', #sip_hdr_cseq{sequence = Sequence, method = Method}};

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
parse_header(Name, Value) when Name =:= 'from'; Name =:= 'to' ->
    {Display, URI, Params} = parse_fromto(sip_binary:trim_leading(Value)),
    {Params2, <<>>} = parse_params(sip_binary:trim_leading(Params), []),
    {Name, #sip_hdr_address{display_name = sip_binary:trim(Display), uri = URI, params = Params2}};

%% Any other header
parse_header(Name, Value) ->
    {Name, Value}.

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
    Transport2 = sip_binary:try_binary_to_existing_atom(sip_binary:to_lower(Transport)),
    {{Protocol, Version, Transport2}, Bin4}.

%% sent-by           =  host [ COLON port ]
parse_sent_by(Bin) ->
    {Host, Bin2} = sip_binary:parse_token(Bin),
    {Port, Bin3} = case Bin2 of
                       <<?HCOLON, Rest/binary>>
                         ->
                           {P, Bin4} = sip_binary:parse_token(Rest),
                           {sip_binary:to_integer(P), Bin4};
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
                {{sip_binary:try_binary_to_existing_atom(Name), Value}, R};

            % Parameter without a value
            {Name, Bin2} ->
                {sip_binary:try_binary_to_existing_atom(Name), Bin2}
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
-spec format_header(sip_headers:header()) -> binary().
%% Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
format_header({'via', Value}) when is_binary(Value) ->
    <<"Via: ", Value/binary, "\r\n">>;

format_header({'via', Parms}) when is_list(Parms) ->
    Bin = sip_binary:join(<<"Via: ">>, fun format_via_parm/2, <<?COMMA>>, Parms),
    <<Bin/binary, "\r\n">>;

%% Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
format_header({'content-length', Length}) when is_binary(Length) ->
    <<"Content-Length: ", Length/binary, "\r\n">>;
format_header({'content-length', Length}) when is_integer(Length) ->
    format_header({'content-length', sip_binary:from_integer(Length)});

%% CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
format_header({'cseq', CSeq}) when is_binary(CSeq) ->
    <<"CSeq: ", CSeq/binary, "\r\n">>;
format_header({'cseq', CSeq}) when is_record(CSeq, sip_hdr_cseq) ->
    SequenceBin = sip_binary:from_integer(CSeq#sip_hdr_cseq.sequence),
    MethodBin = sip_binary:any_to_binary(CSeq#sip_hdr_cseq.method),
    <<"CSeq: ", SequenceBin/binary, " ", MethodBin/binary, "\r\n">>;

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
format_header({Name, Addr}) when
  is_record(Addr, sip_hdr_address), (Name =:= 'from' orelse Name =:= 'to') ->
    Prefix = case Name of 'from' -> <<"From: ">>; _ -> <<"To: ">> end,

    URI = Addr#sip_hdr_address.uri,
    Bin = case Addr#sip_hdr_address.display_name of
              <<>> -> <<Prefix/binary, ?LAQUOT, URI/binary, ?RAQUOT>>;
              DisplayName -> <<Prefix/binary, DisplayName/binary, " ", ?LAQUOT, URI/binary, ?RAQUOT>>
          end,
    Bin2 = append_params(Bin, Addr#sip_hdr_address.params),
    <<Bin2/binary, "\r\n">>;

%% Any other header
format_header({Name, Value}) ->
    Name2 = sip_binary:any_to_binary(Name),
    Value2 = sip_binary:any_to_binary(Value),
    <<Name2/binary, ?HCOLON, ?SP, Value2/binary, "\r\n">>.

%% via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
format_via_parm(Value, Bin) ->
    Version = Value#sip_hdr_via.version,
    Transport = sip_binary:to_upper(sip_binary:any_to_binary(Value#sip_hdr_via.transport)),
    Bin2 = <<Bin/binary, "SIP/", Version/binary, $/, Transport/binary>>,
    Bin3 = case Value#sip_hdr_via.sent_by of
        {Host, undefined} ->
            <<Bin2/binary, ?SP, Host/binary>>;

        {Host, Port} ->
            <<Bin2/binary, ?SP, Host/binary, ?HCOLON, (sip_binary:from_integer(Port))/binary>>
    end,
    append_params(Bin3, Value#sip_hdr_via.params).

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
-spec top_via([sip_headers:header()]) -> #sip_hdr_via{}.
top_via(Headers) when is_list(Headers) ->
    % take top via sent-by
    {'via', Via} = lists:keyfind('via', 1, Headers),
    {'via', [Via2 | _]} = sip_headers:parse_header('via', Via),
    Via2.

-spec top_via_branch([header()]) -> binary() | undefined.
top_via_branch(Headers) ->
    Via = sip_headers:top_via(Headers),
    case lists:keyfind(branch, 1, Via#sip_hdr_via.params) of
        {branch, Branch} -> Branch;
        false -> undefined
    end.

%% @doc
%% Construct Content-Length header.
%% @end
-spec content_length(integer()) -> header().
content_length(Len) when is_integer(Len) ->
    {'content-length', Len}.

%% @doc
%% Construct Via header.
%% @end
-spec via(atom(), via_sent_by() | binary(), [any()]) -> header().
via(Transport, {Host, Port}, Params) when
  is_atom(Transport), is_list(Params) ->
    {'via', [#sip_hdr_via{transport = Transport, sent_by = {Host, Port}, params = Params}]};

via(Transport, Host, Params) ->
    via(Transport, {Host, 5060}, Params).

-spec via([{atom(), via_sent_by() | binary(), [any()]}]) -> header().
via(List) when is_list(List) ->
    Vias = [via(Transport, SentBy, Params) || {Transport, SentBy, Params} <- List],
    Vias2 = [Via || {'via', [Via]} <- Vias],
    {'via', Vias2}.

%% @doc
%% Construct CSeq header.
%% @end
-spec cseq(integer(), method()) -> header().
cseq(Sequence, Method) when is_integer(Sequence) ->
    {'cseq', #sip_hdr_cseq{method = Method, sequence = Sequence}}.


%% @doc
%% Construct From header
%% @end
-spec from(binary(), binary(), list()) -> header().
from(DisplayName, URI, Params) when is_binary(DisplayName), is_binary(URI), is_list(Params) ->
    {'from', #sip_hdr_address{display_name = DisplayName, uri = URI, params = Params}}.

%% @doc
%% Construct To header
%% @end
-spec to(binary(), binary(), list()) -> header().
to(DisplayName, URI, Params) when is_binary(DisplayName), is_binary(URI), is_list(Params) ->
    {'to', #sip_hdr_address{display_name = DisplayName, uri = URI, params = Params}}.


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

%% Short header names support
binary_to_header_name(<<"v">>) -> 'via';
binary_to_header_name(<<"l">>) -> 'content-length';
binary_to_header_name(<<"f">>) -> 'from';
binary_to_header_name(<<"t">>) -> 'to';
binary_to_header_name(Bin) ->
    sip_binary:try_binary_to_existing_atom(Bin).

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
                    {'from', <<"sip:alice@localhost">>}, {'to', <<"sip:bob@localhost">>}],
                   split_binary(<<"l: 5\r\nv: SIP/2.0/UDP localhost\r\nf: sip:alice@localhost\r\nt: sip:bob@localhost">>)),
     ?_assertEqual([{<<"x-custom">>, <<"Nothing">>}, {'content-length', <<"5">>}],
                   split_binary(<<"X-Custom: Nothing\r\nContent-Length: 5">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   split_binary(<<"Subject: I know you're there,\r\n               pick up the phone   \r\n               and talk to me!">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   split_binary(<<"Subject: I know you're there,\r\n\tpick up the phone    \r\n               and talk to me!">>)),

     % Already parsed
     ?_assertEqual({'x-custom2', {parsed, value}},
                   parse_header('x-custom2', {parsed, value})),

     % Custom header
     ?_assertEqual({'x-custom2', <<"custom">>},
                   parse_header('x-custom2', <<"custom">>)),

     % Content-Length
     ?_assertEqual({'content-length', 32543523},
                   parse_header('content-length', <<"32543523">>)),
     ?_assertEqual(<<"Content-Length: 98083\r\n">>,
                   format_header({'content-length', 98083})),
     % CSeq
     ?_assertEqual({'cseq', #sip_hdr_cseq{sequence = 1231, method = 'ACK'}},
                   parse_header('cseq', <<"1231 ACK">>)),
     ?_assertEqual(<<"CSeq: 123453 INVITE\r\n">>,
                   format_header({'cseq', #sip_hdr_cseq{sequence=  123453, method = 'INVITE'}})),
     ?_assertEqual(<<"CSeq: 123453 INVITE\r\n">>,
                   format_header({'cseq', <<"123453 INVITE">>})),

     % From/To
     ?_assertEqual(from(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"Bob  Zert <sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(from(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"sip:bob@biloxi.com ;tag=1928301774">>)),
     ?_assertEqual(from(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"<sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(from(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse_header('from', <<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),

     ?_assertEqual(<<"From: Bob  Zert <sip:bob@biloxi.com>;tag=1928301774\r\n">>,
                   format_header(from(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"From: <sip:bob@biloxi.com>;tag=1928301774\r\n">>,
                   format_header(from(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"From: <sip:bob@biloxi.com>;tag=1928301774\r\n">>,
                   format_header(from(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"From: \"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774\r\n">>,
                   format_header(from(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"To: \"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774\r\n">>,
                   format_header(to(<<"\"Bob Zert\"">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),

     % Via
     ?_assertEqual(via(udp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                   parse_header('via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual(via([{udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]},
                        {tcp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]}]),
                   parse_header('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual(<<"Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds\r\n">>,
                   format_header(via(udp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]))),
     ?_assertEqual(<<"Via: SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com:5060;branch=z9hG4bK776asdhds\r\n">>,
                   format_header(via([{udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]},
                                      {tcp, <<"pc33.atlanta.com">>, [{branch, <<"z9hG4bK776asdhds">>}]}]))),
     ?_assertEqual(<<"Via: SIP/2.0/UDP 127.0.0.1:15060;param=value;flag\r\n">>,
                   format_header({'via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>})),

     ?_assertEqual(begin {'via', [Via]} = via(udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]), Via end,
                   top_via([via([{udp, {<<"127.0.0.1">>, 15060}, [{param, <<"value">>}, flag]},
                                 {tcp, {<<"pc33.atlanta.com">>, undefined}, [{branch, <<"z9hG4bK776asdhds">>}]}]),
                            via(udp, {<<"sip.biloxi.com">>, undefined}, [])])),
     ?_assertEqual(<<"z9hG4bK776asdhds">>,
                   top_via_branch([via([{udp, {<<"127.0.0.1">>, 15060}, [{branch, <<"z9hG4bK776asdhds">>}]},
                                        {tcp, {<<"pc33.atlanta.com">>, undefined}, []}]),
                                   via(udp, {<<"sip.biloxi.com">>, undefined}, [])])),
     ?_assertEqual(undefined, top_via_branch([via(udp, {<<"sip.biloxi.com">>, undefined}, [])])),
     % Formatting
     ?_assertEqual(<<"subject: I know you're there, pick up the phone and talk to me!\r\n">>,
        format_header({'subject', <<"I know you're there, pick up the phone and talk to me!">>}))
    ].

-spec update_top_test_() -> list().
update_top_test_() ->
    CL = content_length(123),
    CSeq = cseq(110, 'INVITE'),
    Via1 = via(udp, {<<"127.0.0.1">>, 5060}, []),
    Via2 = via(tcp, {<<"127.0.0.2">>, 15060}, [{ttl, 4}]),
    Via1Up = via(udp, {<<"localhost">>, 5060}, []),
    Fun = fun ('via', Value) when Via1 =:= {'via', Value} -> {'via', NewVal} = Via1Up, NewVal end,
    [?_assertEqual([CL, Via1Up, Via2],
                   update_top_header('via', Fun, [CL, Via1, Via2])),
     ?_assertEqual([CL, CSeq],
                   update_top_header('via', Fun, [CL, CSeq]))
     ].

-endif.
