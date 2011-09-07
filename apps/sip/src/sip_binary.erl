%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Utility functions to work with binary strings
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_binary).

%% API
-export([trim_leading/1, trim_trailing/1, trim/1, to_lower/1, to_upper/1]).
-export([integer_to_binary/1, float_to_binary/1]).
-export([binary_to_integer/1, binary_to_float/1]).
-export([hexstr_to_binary/1, binary_to_hexstr/1]).
-export([parse_until/2, parse_while/2]).

%% Include files
-include("sip_common.hrl").


%%-----------------------------------------------------------------
%% Conversions upper to/from lower
%%-----------------------------------------------------------------

%% @doc Trim leading whitespaces from the binary string
%% @end
-spec trim_leading(binary()) -> binary().
trim_leading(<<>>) -> <<>>;
trim_leading(Bin) ->
    <<C, Rest/binary>> = Bin,
    case Bin of
        <<C, Rest/binary>> when C =:= $ ; C =:= $\t ; C =:= $\r ; C =:= $\n ->
            trim_leading(Rest);
        _Other -> Bin
    end.

%% @doc Trim trailing whitespaces from the binary string
%% @end
-spec trim_trailing(binary()) -> binary().
trim_trailing(<<>>) -> <<>>;
trim_trailing(Bin) ->
    Sz = size(Bin) - 1,
    case Bin of
        <<Rest:Sz/binary, C>> when C =:= $ ; C =:= $\t ; C =:= $\r ; C =:= $\n ->
            trim_trailing(Rest);
        _Other -> Bin
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

%% Scanning binaries

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

%% Conversions

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

%% @doc Convert binary hex string to binary
%%
%% Convert binary hex string to binary. For example, binary hex string
%% `<<"68656c6c6f">>' will be converted to `<<"hello">>'.
%% <em>Note: binary hex string must have even number of digits</em>
%% @end
-spec hexstr_to_binary(binary()) -> binary().
hexstr_to_binary(<<L, Bin/binary>>) when size(Bin) rem 2 =:= 0 ->
    Byte = int(L),
    hexstr_to_binary(Bin, <<Byte>>);
hexstr_to_binary(Bin) ->
    hexstr_to_binary(Bin, <<>>).

hexstr_to_binary(<<>>, Res) -> Res;
hexstr_to_binary(<<H, L, Rest/binary>>, Res) ->
    Byte = int(H) * 16 + int(L),
    hexstr_to_binary(Rest, <<Res/binary, Byte>>).

%% @doc Convert binary to hex string
%%
%% Convert binary to the hex string. For example, binary string
%% `<<"hello">>' will be converted to hex binary string `<<"68656c6c6f">>'.
%% @end
-spec binary_to_hexstr(binary()) -> binary().
binary_to_hexstr(Bin) ->
    binary_to_hexstr(Bin, <<>>).

binary_to_hexstr(<<>>, Res) -> Res;
binary_to_hexstr(<<Byte, Rest/binary>>, Res) ->
    H = hex(Byte div 16),
    L = hex(Byte rem 16),
    binary_to_hexstr(Rest, <<Res/binary, H, L>>).

hex(N) when N >= 0, N =< 9 -> N + $0;
hex(N) when N >= 10, N =< 15 -> N - 10 + $a.

int(C) when C >= $0, C =< $9 -> C - $0;
int(C) when C >= $a, C =< $z -> C - $a + 10;
int(C) when C >= $A, C =< $Z -> C - $A + 10.


%% @doc Convert floating point number to ASCII binary.
%%
%% <em>Note that at most three digits after floating point are returned</em>
%% @end
-spec float_to_binary(float()) -> binary().
float_to_binary(Float) when is_float(Float) ->
    [Res] = io_lib:format("~.3f", [Float]),
    Bin = list_to_binary(Res),
    Sz1 = size(Bin) - 1,
    Sz2 = Sz1 - 1,
    % Strip last zeros
    case Bin of
        <<R:Sz2/binary, "00">> -> R;
        <<R:Sz1/binary, "0">> -> R;
        R -> R
    end.

%% Tests
-ifdef(TEST).

-spec binary_test_() -> list().
binary_test_() ->
    [% trimming, upper, lower
     ?_assertEqual(<<>>, trim_leading(<<>>)),
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

     % conversions to and from binary
     ?_assertEqual(123, binary_to_integer(<<"123">>)),
     ?_assertEqual(<<"123">>, integer_to_binary(123)),
     ?_assertEqual(-123.25, binary_to_float(<<"-123.25">>)),
     ?_assertEqual(<<"-123.0">>, float_to_binary(-123.0)),
     ?_assertEqual(<<"-123.2">>, float_to_binary(-123.2)),
     ?_assertEqual(<<"-123.25">>, float_to_binary(-123.25)),
     ?_assertEqual(<<"-123.253">>, float_to_binary(-123.253)),
     ?_assertEqual(<<"hello">>, hexstr_to_binary(<<"68656c6C6F">>)),
     ?_assertEqual(<<"68656c6c6f">>, binary_to_hexstr(<<"hello">>)),
     ?_assertEqual(<<"\n">>, hexstr_to_binary(<<"a">>)),
     ?_assertEqual(<<"0a">>, binary_to_hexstr(<<"\n">>)),

     % scanning
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $! end)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_while(<<"some! rest  ">>, fun (C) -> C =/= $$ end)),
     ?_assertEqual({<<"some">>, <<" rest  ">>}, parse_until(<<"some rest  ">>, fun (C) -> C =:= $  end)),
     ?_assertEqual({<<"somerest">>, <<>>}, parse_until(<<"somerest">>, fun (C) -> C =:= $  end)),
     ?_assertEqual({<<"some">>, <<"! rest  ">>}, parse_until(<<"some! rest  ">>, $!)),
     ?_assertEqual({<<"some! rest  ">>, <<>>}, parse_until(<<"some! rest  ">>, $$))
    ].

-endif.
