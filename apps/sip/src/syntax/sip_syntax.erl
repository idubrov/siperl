%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP syntax elements support
%%%
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-20">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_syntax).

%% Exports
-export([parse_token/1, parse_quoted_string/1, quote_string/1, parse_comment_string/1]).
-export([is_digit_char/1, is_alpha_char/1, is_alphanum_char/1, is_unreserved_char/1, is_space_char/1]).
-export([is_token_char/1, is_reserved_char/1, is_user_unreserved_char/1]).
-export([parse_integer/1, parse_ip_address/1, parse_host_port/1]).
-export([format_addr/1]).
-export([parse_name/1, format_name/1]).

%% Includes
-include("../sip_common.hrl").
-include("sip_syntax.hrl").
-include("sip.hrl").

%% API

%% is_* functions

%% @doc Check if character is digit
%% @end
-spec is_digit_char(integer()) -> boolean().
is_digit_char(C) when C >= $0, C =< $9 -> true;
is_digit_char(_C) -> false.

%% @doc Check if character is alpha
%%
%% ```
%% alphanum  =  ALPHA / DIGIT
%% '''
%% @end
-spec is_alpha_char(integer()) -> boolean().
is_alpha_char(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z).

%% @doc Check if character is alphanumeric
%%
%% ```
%% alphanum  =  ALPHA / DIGIT
%% '''
%% @end
-spec is_alphanum_char(integer()) -> boolean().
is_alphanum_char(C) -> is_alpha_char(C) orelse is_digit_char(C).


%% @doc Check if character is `token' character (as specified in RFC 3261 25.1)
%%
%% ```
%% token          =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                   / "_" / "+" / "`" / "'" / "~" )
%% '''
%% @end
-spec is_token_char(integer()) -> boolean().
is_token_char(C) when
  C =:= $- ; C =:= $. ; C =:= $! ;
  C =:= $% ; C =:= $* ; C =:= $_ ;
  C =:= $+ ; C =:= $` ; C =:= $' ;
  C =:= $~ -> true;
is_token_char(C) ->
    is_alphanum_char(C).


%% @doc Check if character is `unreserved' character
%%
%% ```
%% unreserved  =  alphanum / mark
%% mark        =  "-" / "_" / "." / "!" / "~" / "*" / "'"
%%                / "(" / ")"
%% '''
%% @end
-spec is_unreserved_char(integer()) -> boolean().
is_unreserved_char(C) when
  C =:= $- ; C =:= $_ ; C =:= $. ;
  C =:= $! ; C =:= $~ ; C =:= $* ;
  C =:= $' ; C =:= $( ; C =:= $) -> true;
is_unreserved_char(C) ->
    is_alphanum_char(C).

%% @doc Check if character is `user-unreserved' character
%%
%% ```
%% user-unreserved  =  "&" / "=" / "+" / "$" / "," / ";" / "?" / "/"
%% '''
%% @end
-spec is_user_unreserved_char(integer()) -> boolean().
is_user_unreserved_char(C) when
  C =:= $& ; C =:= $= ; C =:= $+ ;
  C =:= $$ ; C =:= $, ; C =:= $; ;
  C =:= $? ; C =:= $/ -> true;
is_user_unreserved_char(_C) -> false.

%% @doc Check if character is `reserved' character
%%
%% ```
%% reserved    =  ";" / "/" / "?" / ":" / "@" / "&" / "=" / "+"
%%                / "$" / ","
%% '''
%% @end
-spec is_reserved_char(integer()) -> boolean().
is_reserved_char(C) when
  C =:= $; ; C =:= $/ ; C =:= $? ;
  C =:= $: ; C =:= $@ ; C =:= $& ;
  C =:= $= ; C =:= $+ ; C =:= $$ ;
  C =:= $, -> true;
is_reserved_char(_C) -> false.

%% @doc Check if character is one of the space characters (space, tab, line feed, carriage return)
%% @end
-spec is_space_char(integer()) -> boolean().
is_space_char(C) when C =:= $ ; C =:= $\t ; C =:= $\r ; C =:= $\n -> true;
is_space_char(_) -> false.

%% Parsing

%% @doc Extract `token' from the UTF-8 binary string
%%
%% Extract <code>token</code> from the UTF-8 binary and return the rest. Note
%% that leading whitespaces are skipped and the rest is returned
%% with leading whitespaces trimmed (rest is either empty binary or
%% starts with non-whitespace character).
%% @end
-spec parse_token(binary()) -> {binary(), binary()}.
parse_token(Bin) ->
    {Token, Rest} = sip_binary:parse_while(sip_binary:trim_leading(Bin), fun is_token_char/1),
    {Token, sip_binary:trim_leading(Rest)}.

%%
%% comment        =  LPAREN *(ctext / quoted-pair / comment) RPAREN
%% ctext          =  %x21-27 / %x2A-5B / %x5D-7E / UTF8-NONASCII
%%                   / LWS
%% quoted-pair    =  "\" (%x00-09 / %x0B-0C
%%                   / %x0E-7F)


%% @doc Parse `comment' from the UTF-8 binary string
%%
%% Extract <code>comment</code> from the UTF-8 binary and return it "as is".
%% @end
-spec parse_comment_string(binary()) -> {CommentStr :: binary(), Rest :: binary()}.
parse_comment_string(Bin) ->
    parse_comment_loop(sip_binary:trim_leading(Bin), <<>>, 0).

parse_comment_loop(<<?RPAREN, Rest/binary>>, Acc, 1) ->
    {<<Acc/binary, ?RPAREN>>, sip_binary:trim_leading(Rest)};
parse_comment_loop(<<?LPAREN, Rest/binary>>, Acc, Depth) ->
    parse_comment_loop(Rest, <<Acc/binary, ?LPAREN>>, Depth + 1);
parse_comment_loop(<<?RPAREN, Rest/binary>>, Acc, Depth) ->
    parse_comment_loop(Rest, <<Acc/binary, ?RPAREN>>, Depth - 1);


parse_comment_loop(<<$\\, C, Rest/binary>>, Acc, Depth) when
  C >= 16#00, C =< 16#09;
  C >= 16#0B, C =< 16#0C;
  C >= 16#0E, C =< 16#7F ->
    parse_comment_loop(Rest, <<Acc/binary, C>>, Depth);
parse_comment_loop(<<C, Rest/binary>>, Acc, Depth) when C =/= $\\ ->
    parse_comment_loop(Rest, <<Acc/binary, C>>, Depth).

%%
%% quoted-pair    =  "\" (%x00-09 / %x0B-0C
%%                   / %x0E-7F)
%% qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
%%                   / UTF8-NONASCII
%% quoted-string  =  SWS DQUOTE *(qdtext / quoted-pair ) DQUOTE

%% @doc Parse `quoted-string' from the UTF-8 binary string
%%
%% Extract <code>quoted-string</code> from the UTF-8 binary, unquote it
%% and return the rest. Note that leading whitespaces are skipped and
%% the rest is returned with leading whitespaces trimmed (rest is
%% either empty binary or starts with non-whitespace character).
%% @end
-spec parse_quoted_string(binary()) -> {QuotedStr :: binary(), Rest :: binary()}.
parse_quoted_string(Bin) ->
    <<?DQUOTE, Bin2/binary>> = sip_binary:trim_leading(Bin),
    parse_quoted_loop(Bin2, <<>>).

parse_quoted_loop(<<?DQUOTE, Rest/binary>>, Acc) ->
    {Acc, sip_binary:trim_leading(Rest)};
parse_quoted_loop(<<$\\, C, Rest/binary>>, Acc) when
  C >= 16#00, C =< 16#09;
  C >= 16#0B, C =< 16#0C;
  C >= 16#0E, C =< 16#7F ->
    parse_quoted_loop(Rest, <<Acc/binary, C>>);
parse_quoted_loop(<<C, Rest/binary>>, Acc) when C =/= $\\ ->
    parse_quoted_loop(Rest, <<Acc/binary, C>>).

%% @doc Generate quoted string
%%
%% Generate valid quoted-string by replacing all `\' and `"' occurences
%% with escaped pair.
%% @end
-spec quote_string(binary()) -> binary().
quote_string(Bin) ->
    quote_string_loop(Bin, <<?DQUOTE>>).

quote_string_loop(<<>>, Acc) ->
    <<Acc/binary, ?DQUOTE>>;
quote_string_loop(<<C, Rest/binary>>, Acc) when C =:= $\\; C =:= $" ->
    quote_string_loop(Rest, <<Acc/binary, $\\, C>>);
quote_string_loop(<<C, Rest/binary>>, Acc) when C >= 16#20 ->
    quote_string_loop(Rest, <<Acc/binary, C>>).

%% @doc Parse `host [":" port]' expressions
%%
%% ```
%% hostport       =  host [ ":" port ]
%% host           =  hostname / IPv4address / IPv6reference
%% hostname       =  *( domainlabel "." ) toplabel [ "." ]
%% domainlabel    =  alphanum
%%                   / alphanum *( alphanum / "-" ) alphanum
%% toplabel       =  ALPHA / ALPHA *( alphanum / "-" ) alphanum
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [ ":" IPv4address ]
%% hexpart        =  hexseq / hexseq "::" [ hexseq ] / "::" [ hexseq ]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
%% port           =  1*DIGIT
%% '''
%% @end
-spec parse_host_port(binary()) -> {Host :: string() | inet:ip_address(), Port :: integer() | 'undefined', Rest :: binary()}.
parse_host_port(<<"[", Bin/binary>>) ->
    % IPv6 reference
    IsValidChar = fun ($:) -> true; (C) -> is_alphanum_char(C) end,
    {HostBin, <<"]", Rest/binary>>} = sip_binary:parse_while(Bin, IsValidChar),
    {ok, IPv6} = parse_ip_address(HostBin),
    host_port(IPv6, Rest);
parse_host_port(Bin) ->
    % host name or IPv4
    IsValidChar = fun ($-) -> true; ($.) -> true; (C) -> is_alphanum_char(C) end,
    {HostBin, Rest} = sip_binary:parse_while(Bin, IsValidChar),
    case maybe_ipv4(HostBin) of
        true -> {ok, Host} = parse_ip_address(HostBin);
        false -> Host = binary_to_list(HostBin)
    end,
    host_port(Host, Rest).

host_port(Host, <<":", PortBin/binary>>) ->
    {Port, Rest} = parse_integer(PortBin),
    {Host, Port, Rest};
host_port(HostBin, Rest) ->
    {HostBin, undefined, Rest}.

maybe_ipv4(<<>>) -> true;
maybe_ipv4(<<C, Rest/binary>>) when
  C =:= $. ;
  C >= $0, C =< $9 ->
    maybe_ipv4(Rest);
maybe_ipv4(_) -> false.

%% @doc Parse IP address
%%
%% ```
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
%% IPv6address    =  hexpart [ ":" IPv4address ]
%% hexpart        =  hexseq / hexseq "::" [ hexseq ] / "::" [ hexseq ]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
%% '''
%% @end
-spec parse_ip_address(binary()) -> {ok, inet:ip_address()} | {error, Reason :: term()}.
parse_ip_address(Bin) ->
    inet_parse:address(binary_to_list(Bin)).


%% @doc Convert address (IPv4, IPv6 or hostname) to binary string
%% @end
-spec format_addr(inet:ip_address() | string()) -> binary().
format_addr(Str) when is_list(Str) -> list_to_binary(Str);
format_addr({A, B, C, D}) when
  is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    <<(sip_binary:integer_to_binary(A))/binary, $.,
      (sip_binary:integer_to_binary(B))/binary, $.,
      (sip_binary:integer_to_binary(C))/binary, $.,
      (sip_binary:integer_to_binary(D))/binary>>;
format_addr({A, B, C, D, E, F, G, H}) when
  is_integer(A), is_integer(B), is_integer(C), is_integer(D),
  is_integer(E), is_integer(F), is_integer(G), is_integer(H) ->
    Bin1 = hex4(A, <<"[">>),
    Bin2 = hex4(B, <<Bin1/binary, ":">>),
    Bin3 = hex4(C, <<Bin2/binary, ":">>),
    Bin4 = hex4(D, <<Bin3/binary, ":">>),
    Bin5 = hex4(E, <<Bin4/binary, ":">>),
    Bin6 = hex4(F, <<Bin5/binary, ":">>),
    Bin7 = hex4(G, <<Bin6/binary, ":">>),
    Bin8 = hex4(H, <<Bin7/binary, ":">>),
    <<Bin8/binary, "]">>.


hex4(N, Bin) ->
    A = hex(N div 4096),
    B = hex((N rem 4096) div 256),
    C = hex((N rem 256) div 16),
    D = hex(N rem 16),
    <<Bin/binary, A, B, C, D>>.

hex(N) when N >= 0, N =< 9 -> N + $0;
hex(N) when N >= 10, N =< 15 -> N - 10 + $a.

%% @doc Parse number from the binary and return the rest
%% @end
-spec parse_integer(binary()) -> {integer(), binary()}.
parse_integer(<<C, _/binary>> = Bin) when C >= $0, C =< $9 ->
    parse_integer(Bin, 0).

parse_integer(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_integer(Rest, Acc * 10 + (C - $0));
parse_integer(<<Rest/binary>>, Acc) ->
    {Acc, Rest}.

%% @doc Parse binary into name
%%
%% If atom matching the binary name exist, atom is returned. Otherwise,
%% binary is returned as-is.
%% @end
-spec parse_name(binary()) -> sip_name().
parse_name(Bin) ->
    try binary_to_existing_atom(Bin, utf8)
    catch error:badarg -> Bin
    end.


%% @doc Convert name to the binary
%% @end
-spec format_name(sip_name()) -> binary().
format_name(Name) when is_binary(Name) -> Name;
format_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8).

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec syntax_test_() -> list().
syntax_test_() ->
    [% quoting
     ?_assertEqual(<<"\"va\\\\lue\"">>, quote_string(<<"va\\lue">>)),

     % "is" tests
     ?_assertEqual(true, is_digit_char($5)),
     ?_assertEqual(false, is_digit_char($a)),
     ?_assertEqual(true, is_alpha_char($T)),
     ?_assertEqual(false, is_alpha_char($5)),
     ?_assertEqual(true, is_alphanum_char($a)),
     ?_assertEqual(false, is_alphanum_char($:)),
     ?_assertEqual(true, is_unreserved_char($~)),
     ?_assertEqual(false, is_unreserved_char($;)),
     ?_assertEqual(true, is_space_char($ )),
     ?_assertEqual(false, is_space_char($b)),
     ?_assertEqual(true, is_token_char($+)),
     ?_assertEqual(false, is_token_char($:)),
     ?_assertEqual(true, is_reserved_char($;)),
     ?_assertEqual(false, is_reserved_char($~)),
     ?_assertEqual(true, is_user_unreserved_char($&)),
     ?_assertEqual(false, is_user_unreserved_char($~)),

     % parse token, quoted strings and comments
     ?_assertEqual({<<"some">>, <<"data ">>}, parse_token(<<"   some data ">>)),
     ?_assertEqual({<<"some">>, <<>>}, parse_token(<<"   some">>)),
     ?_assertEqual({<<"some">>, <<"=value">>}, parse_token(<<"some=value">>)),
     ?_assertEqual({<<"some">>, <<"data ">>}, parse_quoted_string(<<"   \"some\" data ">>)),
     ?_assertEqual({<<"so\"me">>, <<"data ">>}, parse_quoted_string(<<"   \"so\\\"me\" data ">>)),
     ?_assertEqual({<<"(some)">>, <<"data ">>}, parse_comment_string(<<"   (some) data ">>)),
     ?_assertEqual({<<"(so\"me)">>, <<"data ">>}, parse_comment_string(<<"   (so\\\"me) data ">>)),
     ?_assertEqual({<<"(some (nested) comment)">>, <<"data ">>}, parse_comment_string(<<"   (some (nested) comment) data ">>)),

     % parsing/formatting names
     ?_assertEqual(<<"neverexistingatom">>, parse_name(<<"neverexistingatom">>)),
     ?_assertEqual('existingatom', parse_name(<<"existingatom">>)),
     ?_assertEqual(<<"neverexistingatom">>, format_name(<<"neverexistingatom">>)),
     ?_assertEqual(<<"existingatom">>, format_name('existingatom')),

     % host[:port]
     ?_assertEqual({"atlanta.com", undefined, <<";param">>}, parse_host_port(<<"atlanta.com;param">>)),
     ?_assertEqual({{127, 0, 0, 1}, 5060, <<";param">>}, parse_host_port(<<"127.0.0.1:5060;param">>)),
     ?_assertEqual({{8193,3512,0,0,0,0,44577,44306}, 5060, <<";param">>},
                   parse_host_port(<<"[2001:0db8:0000:0000:0000:0000:ae21:ad12]:5060;param">>)),

     % formatting address
     ?_assertEqual(<<"example.com">>, format_addr("example.com")),
     ?_assertEqual(<<"10.0.0.1">>, format_addr({10, 0, 0, 1})),
     ?_assertEqual(<<"[2001:0db8:11a3:09d7:1f34:8a2e:07a0:765d]">>, format_addr({8193,3512,4515,2519,7988,35374,1952,30301}))

    ].

-endif.
