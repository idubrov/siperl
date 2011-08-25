%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Utility functions to work with binary strings
%%%
%%% Parsing SIP basic elements, checking if character is in given
%%% class, etc.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_binary).

%% API
-export([trim_leading/1, trim_trailing/1, trim/1, to_lower/1, to_upper/1]).
-export([parse_until/2, parse_while/2]).
-export([parse_token/1, parse_quoted_string/1, quote_string/1]).
-export([integer_to_binary/1, float_to_binary/1, any_to_binary/1, addr_to_binary/1]).
-export([binary_to_integer/1, binary_to_float/1, binary_to_existing_atom/1]).
-export([is_digit_char/1, is_alphanum_char/1, is_unreserved_char/1, is_space_char/1]).
-export([is_token_char/1, is_reserved_char/1, is_user_unreserved_char/1]).
-export([parse_number/1, parse_ip_address/1, parse_host_port/1]).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip_parse.hrl").

%%-----------------------------------------------------------------
%% is_* functions
%%-----------------------------------------------------------------

%% @doc Check if character is digit
%% @end
-spec is_digit_char(integer()) -> boolean().
is_digit_char(C) when C >= $0, C =< $9 -> true;
is_digit_char(_C) -> false.

%% @doc Check if character is alphanumeric
%%
%% ```
%% alphanum  =  ALPHA / DIGIT
%% '''
%% @end
-spec is_alphanum_char(integer()) -> boolean().
is_alphanum_char(C) when C >= $a, C =< $z; C >= $A, C =< $Z -> true;
is_alphanum_char(C) -> is_digit_char(C).


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

%%-----------------------------------------------------------------
%% Conversions upper to/from lower
%%-----------------------------------------------------------------

%% @doc Trim leading whitespaces from the binary string
%% @end
-spec trim_leading(binary()) -> binary().
trim_leading(<<>>) -> <<>>;
trim_leading(Bin) ->
    <<C, Rest/binary>> = Bin,
    IsSpace = is_space_char(C),
    if
        IsSpace -> trim_leading(Rest);
        true -> Bin
    end.

%% @doc Trim trailing whitespaces from the binary string
%% @end
-spec trim_trailing(binary()) -> binary().
trim_trailing(<<>>) -> <<>>;
trim_trailing(Bin) ->
    Sz = size(Bin) - 1,
    <<Rest:Sz/binary, C>> = Bin,
    case is_space_char(C) of
        true -> trim_trailing(Rest);
        _ -> Bin
    end.

%% @doc Trim both trailing and leading whitespaces from the binary string
%% @end
-spec trim(binary()) -> binary().
trim(Bin) ->
    trim_trailing(trim_leading(Bin)).

%% @doc Convert binary UTF-8 encoded string to lowercase. Note that only
%% latin1 characters are actually converted.
%% @end
-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
    << <<(string:to_lower(Char))/utf8>> || <<Char/utf8>> <= Bin >>.

%% @doc Convert binary UTF-8 encoded string to uppercase. Note that only
%% latin1 characters are actually converted.
%% @end
-spec to_upper(binary()) -> binary().
to_upper(Bin) ->
    << <<(string:to_upper(Char))/utf8>> || <<Char/utf8>> <= Bin >>.


%%-----------------------------------------------------------------
%% Parse functions
%%-----------------------------------------------------------------

%% @doc Parse number from the binary and return the rest
%% @end
-spec parse_number(binary()) -> {integer(), binary()}.
parse_number(<<C, _/binary>> = Bin) when C >= $0, C =< $9 ->
    parse_number(Bin, 0).

parse_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    parse_number(Rest, Acc * 10 + (C - $0));
parse_number(<<Rest/binary>>, Acc) ->
    {Acc, Rest}.

%% @doc Parse binary while given predicate function evaluates to `true'
%% @end
-spec parse_while(binary(), fun((char()) -> boolean())) -> {Result :: binary(), Rest :: binary()}.
parse_while(Bin, Fun) ->
    parse_while(Bin, Fun, 0).

parse_while(Bin, _Fun, Pos) when Pos =:= size(Bin) ->
    {Bin, <<>>};
parse_while(Bin, Fun, Pos) when is_function(Fun) ->
    <<Start:Pos/binary, Char, Rest/binary>> = Bin,
    case Fun(Char) of
        false ->
            {Start, <<Char, Rest/binary>>};
        _ ->
            parse_while(Bin, Fun, Pos + 1)
    end.
%% @doc Parse binary until given predicate function evaluates to `true' or until given character is encountered
%% @end
-spec parse_until(binary(), fun((char()) -> boolean()) | char()) -> {Result :: binary(), Rest :: binary()}.
parse_until(Bin, Char) when is_integer(Char) ->
    parse_while(Bin, fun (C) -> C =/= Char end, 0);
parse_until(Bin, Fun) when is_function(Fun) ->
    parse_while(Bin, fun (C) -> not Fun(C) end, 0).

%% @doc Extract `token' from the UTF-8 binary string
%%
%% Extract <code>token</code> from the UTF-8 binary and return the rest. Note
%% that leading whitespaces are skipped and the rest is returned
%% with leading whitespaces trimmed (rest is either empty binary or
%% starts with non-whitespace character).
%% @end
-spec parse_token(binary()) -> {binary(), binary()}.
parse_token(Bin) ->
    {Token, Rest} = parse_while(trim_leading(Bin), fun is_token_char/1, 0),
    {Token, trim_leading(Rest)}.

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
    <<?DQUOTE, Bin2/binary>> = trim_leading(Bin),
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
    {HostBin, <<"]", Rest/binary>>} = parse_while(Bin, IsValidChar),
    {ok, IPv6} = parse_ip_address(HostBin),
    host_port(IPv6, Rest);
parse_host_port(Bin) ->
    % host name or IPv4
    IsValidChar = fun ($-) -> true; ($.) -> true; (C) -> is_alphanum_char(C) end,
    {HostBin, Rest} = parse_while(Bin, IsValidChar),
    case maybe_ipv4(HostBin) of
        true -> {ok, Host} = parse_ip_address(HostBin);
        false -> Host = binary_to_list(HostBin)
    end,
    host_port(Host, Rest).

host_port(Host, <<":", PortBin/binary>>) ->
    {Port, Rest} = sip_binary:parse_number(PortBin),
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

%% @doc Convert UTF-8 binary to integer
%% @end
-spec binary_to_integer(binary()) -> integer().
binary_to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% @doc Convert integer to ASCII binary.
%% @end
-spec integer_to_binary(integer()) -> binary().
integer_to_binary(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).

%% @doc Convert UTF-8 binary to floating point number
%% @end
-spec binary_to_float(binary()) -> float().
binary_to_float(Bin) when is_binary(Bin) ->
    list_to_float(binary_to_list(Bin)).

%% @doc Convert floating point number to ASCII binary.
%%
%% <em>Note that only three digits after floating point are returned</em>
%% @end
-spec float_to_binary(float()) -> binary().
float_to_binary(Float) when is_float(Float) ->
    [Res] = io_lib:format("~.3f", [Float]),
    list_to_binary(Res).

%% @doc Convert binary to existing atom
%%
%% Tries to convert binary to existing atom. If such atom does not exist,
%% binary is returned as is.
%% @end
-spec binary_to_existing_atom(binary()) -> atom() | binary().
binary_to_existing_atom(Bin) ->
    try binary_to_existing_atom(Bin, utf8)
    catch error:badarg -> Bin
    end.

%% @doc Convert atoms, binaries, integers or strings to binary.
%% @end
-spec any_to_binary(atom() | binary() | integer() | list() | inet:ip_address()) -> binary().
any_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
any_to_binary(Bin) when is_binary(Bin) -> Bin;
any_to_binary(Int) when is_integer(Int) -> integer_to_binary(Int);
any_to_binary(List) when is_list(List) -> list_to_binary(List);
any_to_binary(MaybeAddr) -> addr_to_binary(MaybeAddr).


%% @doc Convert address (IPv4, IPv6 or hostname) to binary string
%% @end
-spec addr_to_binary(inet:ip_address() | string()) -> binary().
addr_to_binary(Str) when is_list(Str) -> list_to_binary(Str);
addr_to_binary({A, B, C, D}) when
  is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    <<(integer_to_binary(A))/binary, $.,
      (integer_to_binary(B))/binary, $.,
      (integer_to_binary(C))/binary, $.,
      (integer_to_binary(D))/binary>>;
addr_to_binary({A, B, C, D, E, F, G, H}) when
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


%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec binary_test_() -> list().
binary_test_() ->
    % for join tests
    [?_assertEqual(<<>>, trim_leading(<<>>)),
     ?_assertEqual(<<>>, trim_leading(<<"    ">>)),
     ?_assertEqual(<<"ABC DEF    ">>, trim_leading(<<"  ABC DEF    ">>)),
     ?_assertEqual(<<>>, trim_trailing(<<>>)),
     ?_assertEqual(<<>>, trim_trailing(<<"    ">>)),
     ?_assertEqual(<<"  ABC DEF">>, trim_trailing(<<"  ABC DEF    ">>)),
     ?_assertEqual(<<>>, trim(<<>>)),
     ?_assertEqual(<<>>, trim(<<"    ">>)),
     ?_assertEqual(<<"ABC DEF">>, trim(<<"  ABC DEF    ">>)),
     ?_assertEqual(<<"hello">>, to_lower(<<"HELlo">>)),
     ?_assertEqual(<<"HELLO">>, to_upper(<<"HELlo">>)),
     ?_assertEqual(true, is_token_char($+)),
     ?_assertEqual(false, is_token_char($:)),

     % parse token and quoted strings
     ?_assertEqual({<<"some">>, <<"data ">>}, parse_token(<<"   some data ">>)),
     ?_assertEqual({<<"some">>, <<>>}, parse_token(<<"   some">>)),
     ?_assertEqual({<<"some">>, <<"=value">>}, parse_token(<<"some=value">>)),
     ?_assertEqual({<<"some">>, <<"data ">>}, parse_quoted_string(<<"   \"some\" data ">>)),
     ?_assertEqual({<<"so\"me">>, <<"data ">>}, parse_quoted_string(<<"   \"so\\\"me\" data ">>)),

     % conversions to and from binary
     ?_assertEqual(123, binary_to_integer(<<"123">>)),
     ?_assertEqual(<<"123">>, integer_to_binary(123)),
     ?_assertEqual(-123.25, binary_to_float(<<"-123.25">>)),
     ?_assertEqual(<<"-123.250">>, float_to_binary(-123.25)),
     ?_assertEqual(<<"some">>, any_to_binary(some)),
     ?_assertEqual(<<"some">>, any_to_binary(<<"some">>)),
     ?_assertEqual(<<"123">>, any_to_binary(123)),
     ?_assertEqual(<<"some">>, any_to_binary("some")),
     ?_assertEqual(<<"10.0.0.1">>, any_to_binary({10, 0, 0, 1})),
     ?_assertEqual(<<"10.0.0.1">>, addr_to_binary({10, 0, 0, 1})),
     ?_assertEqual(<<"neverexistingatom">>, binary_to_existing_atom(<<"neverexistingatom">>)),
     ?_assertEqual('existingatom', binary_to_existing_atom(<<"existingatom">>)),

     % parse
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $! end)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $$ end)),
     ?_assertEqual({<<"some">>, <<" rest  ">>}, parse_until(<<"some rest  ">>, fun is_space_char/1)),
     ?_assertEqual({<<"somerest">>, <<>>}, parse_until(<<"somerest">>, fun is_space_char/1)),
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_until(<<"some! rest  ">>, $!)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_until(<<"some! rest  ">>, $$)),

     % host[:port]
     ?_assertEqual({"atlanta.com", undefined, <<";param">>}, parse_host_port(<<"atlanta.com;param">>)),
     ?_assertEqual({{127, 0, 0, 1}, 5060, <<";param">>}, parse_host_port(<<"127.0.0.1:5060;param">>)),
     ?_assertEqual({{8193,3512,0,0,0,0,44577,44306}, 5060, <<";param">>},
                   parse_host_port(<<"[2001:0db8:0000:0000:0000:0000:ae21:ad12]:5060;param">>))
    ].

-endif.
