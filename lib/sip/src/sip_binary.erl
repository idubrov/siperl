%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc Utility functions to work with binary strings.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_binary).

%% API
-export([trim_leading/1, trim_trailing/1, trim/1, to_lower/1, to_upper/1]).
-export([parse_until/2, parse_while/2]).
-export([parse_token/1, parse_quoted_string/1, parse_token_or_quoted_string/1]).
-export([binary_to_integer/1, integer_to_binary/1, binary_to_existing_atom/1, any_to_binary/1, ip_to_binary/1]).
-export([is_space_char/1, is_token_char/1, parse_ip_address/1]).
-export([parse_host_port/1]).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip_parse.hrl").

%%-----------------------------------------------------------------
%% API functions
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


%% token          =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                   / "_" / "+" / "`" / "'" / "~" )

-spec is_token_char(integer()) -> boolean().
%% @doc Check if character is `token' character (as specified in RFC 3261 25.1)
%% @end
is_token_char(C) when
  (C >= $0 andalso C =< $9) ;
  (C >= $a andalso C =< $z) ;
  (C >= $A andalso C =< $Z) ;
  C =:= $- ; C =:= $. ; C =:= $! ; C =:= $% ; C =:= $* ;
  C =:= $_ ; C =:= $+ ; C =:= $` ; C =:= $' ; C =:= $~ ->
    true;

is_token_char(_) ->
    false.

%% @doc Check if character is one of the space characters (space, tab, line feed, carriage return)
%% @end
-spec is_space_char(integer()) -> boolean().
is_space_char(C) when C =:= $ ; C =:= $\t ; C =:= $\r ; C =:= $\n -> true;
is_space_char(_) -> false.

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
%% Extract <code>quoted-string</code> from the UTF-8 binary and return the rest.
%% Note that leading whitespaces are skipped and the rest is returned
%% with leading whitespaces trimmed (rest is either empty binary or
%% starts with non-whitespace character).
%% @end
-spec parse_quoted_string(binary()) -> {QuotedStr :: binary(), Rest :: binary()}.
parse_quoted_string(Bin) ->
    parse_quoted_string_internal(trim_leading(Bin)).
parse_quoted_string_internal(<<?DQUOTE, _/binary>> = Bin) ->
    parse_quoted_string(Bin, 1).

parse_quoted_string(Bin, Pos) when Pos =:= size(Bin) ->
    {<<>>, Bin};
parse_quoted_string(Bin, Pos) ->
    <<Str:Pos/binary, C, Rest/binary>> = Bin,
    if
        C =:= ?DQUOTE ->
            {<<Str/binary, C>>, trim_leading(Rest)};
        C =:= $\\ ->
            parse_quoted_string(Bin, Pos + 2);
        true ->
            parse_quoted_string(Bin, Pos + 1)
    end.

%% @doc Parse either `token' or `quoted-string' from the UTF-8 binary string
%% @end
-spec parse_token_or_quoted_string(binary()) -> {Result :: binary(), Rest :: binary()}.
parse_token_or_quoted_string(Bin) ->
    case trim_leading(Bin) of
        <<?DQUOTE, _/binary>> ->
            parse_quoted_string(Bin);
        _ ->
            parse_token(Bin)
    end.


%% @doc Parse `host [":" port]' expressions
%% @end
-spec parse_host_port(binary()) -> {Host :: binary(), Port :: integer(), Rest :: binary()}.
parse_host_port(Bin) ->
    {Host, Bin2} = sip_binary:parse_token(Bin),
    {Port, Bin3} = case Bin2 of
                       <<":", Rest/binary>> ->
                           {P, Bin4} = sip_binary:parse_token(Rest),
                           {sip_binary:binary_to_integer(P), Bin4};
                       _ ->
                           {undefined, Bin2}
                   end,
    {Host, Port, Bin3}.


%% @doc Convert UTF-8 binary to integer
%% @end
-spec binary_to_integer(binary()) -> integer().
binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

%% @doc Convert integer to ASCII binary.
%% @end
-spec integer_to_binary(integer()) -> binary().
integer_to_binary(Int) ->
    list_to_binary(integer_to_list(Int)).

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
any_to_binary({_, _, _, _} = Ip) -> ip_to_binary(Ip).


%% @doc Convert IP address to binary string
%% @end
-spec ip_to_binary(inet:ip_address()) -> binary().
ip_to_binary({A, B, C, D}) when
  is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    <<(integer_to_binary(A))/binary, $.,
      (integer_to_binary(B))/binary, $.,
      (integer_to_binary(C))/binary, $.,
      (integer_to_binary(D))/binary>>.

%% @doc Check if given binary is ip address
%%
%% @end
-spec parse_ip_address(binary()) -> {ok, inet:ip_address()} | {error, invalid}.
parse_ip_address(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<$.>>, [global]) of
        List when length(List) =:= 4 ->
            Addr = [parse_ipv4_octet(Octet) || Octet <- List],
            case lists:all(fun (false) -> false; (_) -> true end, Addr) of
                true -> {ok, list_to_tuple(Addr)};
                false -> {error, invalid}
            end;
        _ -> false
    end.

%% @doc Parse given binary string as IPv4 octet.
%%
%% Returns `false' if binary string is invalid (not an integer or out of range).
%% @end
parse_ipv4_octet(<<>>) -> false;
parse_ipv4_octet(Bin) -> parse_ipv4_octet(Bin, 0).

parse_ipv4_octet(<<>>, Acc) when Acc >= 0, Acc =< 255 -> Acc;
parse_ipv4_octet(<<Char, Rest/binary>>, Acc)
  when Char >= $0, Char =< $9 ->
    parse_ipv4_octet(Rest, Acc * 10 + (Char - $0));
parse_ipv4_octet(_Char, _Acc) -> false.


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
     ?_assertEqual({<<"\"some\"">>, <<"data ">>}, parse_quoted_string(<<"   \"some\" data ">>)),
     ?_assertEqual({<<>>, <<"\"some">>}, parse_quoted_string(<<"   \"some">>)),
     ?_assertEqual({<<"\"so\\nme\"">>, <<"data ">>}, parse_quoted_string(<<"   \"so\\nme\" data ">>)),
     ?_assertEqual({<<"some">>, <<"data ">>}, parse_token_or_quoted_string(<<"   some data ">>)),
     ?_assertEqual({<<"some">>, <<>>}, parse_token_or_quoted_string(<<"   some">>)),
     ?_assertEqual({<<"\"some\"">>, <<"data ">>}, parse_token_or_quoted_string(<<"   \"some\" data ">>)),
     ?_assertEqual({<<>>, <<"\"some">>}, parse_token_or_quoted_string(<<"   \"some">>)),
     ?_assertEqual({<<"\"so\\nme\"">>, <<"data ">>}, parse_token_or_quoted_string(<<"   \"so\\nme\" data ">>)),

     % conversions to and from binary
     ?_assertEqual(123, binary_to_integer(<<"123">>)),
     ?_assertEqual(<<"123">>, integer_to_binary(123)),
     ?_assertEqual(<<"some">>, any_to_binary(some)),
     ?_assertEqual(<<"some">>, any_to_binary(<<"some">>)),
     ?_assertEqual(<<"123">>, any_to_binary(123)),
     ?_assertEqual(<<"some">>, any_to_binary("some")),
     ?_assertEqual(<<"10.0.0.1">>, any_to_binary({10, 0, 0, 1})),
     ?_assertEqual(<<"10.0.0.1">>, ip_to_binary({10, 0, 0, 1})),
     ?_assertEqual(<<"neverexistingatom">>, binary_to_existing_atom(<<"neverexistingatom">>)),
     ?_assertEqual('existingatom', binary_to_existing_atom(<<"existingatom">>)),

     % parse
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $! end)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $$ end)),
     ?_assertEqual({<<"some">>, <<" rest  ">>}, parse_until(<<"some rest  ">>, fun is_space_char/1)),
     ?_assertEqual({<<"somerest">>, <<>>}, parse_until(<<"somerest">>, fun is_space_char/1)),
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_until(<<"some! rest  ">>, $!)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_until(<<"some! rest  ">>, $$))

     % FIXME: parse_host_port!
    ].

-endif.
