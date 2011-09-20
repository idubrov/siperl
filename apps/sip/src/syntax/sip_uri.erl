%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc URI parse utilities
%%%
%%% Implementation of parsing for supported URI schemes (`sip', `sips', `tel').
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-19">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_uri).

%% Exports

%% API
-export([parse/1, format/1]).
-export([is_loose_router/1, is_strict_router/1, is_sips/1]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------


%% @doc Parses all supported URI formats
%%
%% If URI could not be parsed because scheme is not supported,
%% a binary is returned as is.
%% @end
-spec parse(URI :: binary()) -> #sip_uri{} | binary().
parse(<<"sip:", Rest/binary>>) -> parse_sip(sip, Rest);
parse(<<"sips:", Rest/binary>>) -> parse_sip(sips, Rest);
parse(Bin) when is_binary(Bin) ->
    Bin.

%% @doc Check if SIP URI is loose router URI
%% @end
-spec is_loose_router(#sip_uri{}) -> boolean().
is_loose_router(#sip_uri{params = Params}) ->
    proplists:get_bool(lr, Params).

%% @doc Check if SIP URI is strict router URI
%% @end
-spec is_strict_router(#sip_uri{}) -> boolean().
is_strict_router(URI) ->
    not is_loose_router(URI).

-spec is_sips(sip_uri()) -> boolean.
%% @doc Check if URI is SIPS URI.
%% @end
is_sips(#sip_uri{scheme = sips}) -> true;
is_sips(_Other) -> false.


%% @doc Parse SIP URI
%% BNF notation:
%% ```
%% SIP-URI          =  "sip:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
%% SIPS-URI         =  "sips:" [ userinfo ] hostport
%%                     uri-parameters [ headers ]
%% userinfo         =  ( user / telephone-subscriber ) [ ":" password ] "@"
%% user             =  1*( unreserved / escaped / user-unreserved )
%% hostport         =  host [ ":" port ]
%% uri-parameters   =  *( ";" uri-parameter)
%% uri-parameter    =  transport-param / user-param / method-param
%%                     / ttl-param / maddr-param / lr-param / other-param
%% headers          =  "?" header *( "&" header )
%% header           =  hname "=" hvalue
%% '''
%% @end
parse_sip(Scheme, Bin) ->
    case binary:split(Bin, <<$@>>) of
        [UserInfo, Rest] ->
            case binary:split(UserInfo, <<$:>>) of
                [User, Password] -> ok;
                [User] -> Password = <<>>
            end;
        [Rest] ->
            User = <<>>,
            Password = <<>>
    end,
    {Host, Port, ParamsBin} = sip_syntax:parse_host_port(Rest),
    {Params, HeadersBin} = parse_params(ParamsBin, []),
    Headers = parse_headers(HeadersBin),
    #sip_uri{scheme = Scheme,
             user = User,
             password = Password,
             host = Host,
             port = Port,
             headers = Headers,
             params = Params}.


%% @doc Parse SIP URI parameters lists
%% BNF notation:
%% ```
%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param
%%                      / ttl-param / maddr-param / lr-param / other-param
%% transport-param   =  "transport="
%%                      ( "udp" / "tcp" / "sctp" / "tls"
%%                      / other-transport)
%% other-transport   =  token
%% user-param        =  "user=" ( "phone" / "ip" / other-user)
%% other-user        =  token
%% method-param      =  "method=" Method
%% ttl-param         =  "ttl=" ttl
%% maddr-param       =  "maddr=" host
%% lr-param          =  "lr"
%% other-param       =  pname [ "=" pvalue ]
%% pname             =  1*paramchar
%% pvalue            =  1*paramchar
%% paramchar         =  param-unreserved / unreserved / escaped
%% param-unreserved  =  "[" / "]" / "/" / ":" / "&" / "+" / "$"
%% '''
%% @end
parse_params(<<$;, Bin/binary>>, List) ->
    Pred = fun ($;) -> true; % next parameter
               ($?) -> true; % headers
               ($=) -> true; % value
               (_) -> false
           end,
    {Param, Rest} =
        case sip_binary:parse_until(Bin, Pred) of
            {Name, <<$=, Bin2/binary>>} ->
                % parse value
                {Value, R} = sip_binary:parse_until(Bin2, Pred),
                {{sip_syntax:parse_name(Name), Value}, R};
            {Name, Bin2} ->
                % no value
                {sip_syntax:parse_name(Name), Bin2}
        end,
    parse_params(Rest, [Param|List]);
parse_params(Bin, List) ->
    {lists:reverse(List), Bin}.

%% @doc Parse SIP URI headers
%% BNF notation:
%% ```
%% headers         =  "?" header *( "&" header )
%% header          =  hname "=" hvalue
%% hname           =  1*( hnv-unreserved / unreserved / escaped )
%% hvalue          =  *( hnv-unreserved / unreserved / escaped )
%% hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
%% unreserved      =  alphanum / mark
%% mark            =  "-" / "_" / "." / "!" / "~" / "*" / "'"
%%                    / "(" / ")"
%% escaped         =  "%" HEXDIG HEXDIG
%% '''
%% @end
parse_headers(<<>>) -> [];
parse_headers(<<$?, Bin/binary>>) ->
    Headers = [binary:split(Header, <<$=>>) || Header <- binary:split(Bin, <<$&>>)],
    [{sip_syntax:parse_name(Name), Value} ||
     [Name, Value] <- Headers].

%% @doc Format URI to binary string
%%
%% @end
-spec format(#sip_uri{} | binary()) -> binary().
format(URI) when is_binary(URI) -> URI;
format(#sip_uri{scheme = sip} = URI) -> append_userinfo(URI, <<"sip:">>);
format(#sip_uri{scheme = sips} = URI) -> append_userinfo(URI, <<"sips:">>).

append_userinfo(#sip_uri{user = <<>>} = URI, Bin) -> append_hostport(URI, Bin);
append_userinfo(#sip_uri{user = User} = URI, Bin) -> append_password(URI, <<Bin/binary, User/binary>>).

append_password(#sip_uri{password = <<>>} = URI, Bin) ->     append_hostport(URI, <<Bin/binary, $@>>);
append_password(#sip_uri{password = Password} = URI, Bin) -> append_hostport(URI, <<Bin/binary, $:, Password/binary, $@>>).

append_hostport(URI, Bin) ->
    Host = sip_syntax:format_addr(URI#sip_uri.host),
    append_port(URI, <<Bin/binary, Host/binary>>).

append_port(#sip_uri{port = undefined} = URI, Bin) -> append_params(URI, URI#sip_uri.params, Bin);
append_port(#sip_uri{port = Port} = URI, Bin) ->      append_params(URI, URI#sip_uri.params, <<Bin/binary, $:, (sip_binary:integer_to_binary(Port))/binary>>).

append_params(URI, [{Name, Value} | Tail], Bin) ->
    NameBin = sip_syntax:format_name(Name),
    Bin2 = <<Bin/binary, $;, NameBin/binary, $=, Value/binary>>,
    append_params(URI, Tail, Bin2);
append_params(URI, [Name | Tail], Bin) ->
    NameBin = sip_syntax:format_name(Name),
    Bin2 = <<Bin/binary, $;, NameBin/binary>>,
    append_params(URI, Tail, Bin2);
append_params(URI, [], Bin) -> append_headers(URI#sip_uri.headers, $?, Bin).

append_headers([], _Sep, Bin) -> Bin;
append_headers([{Name, Value} | Tail], Sep, Bin) ->
    NameBin = sip_syntax:format_name(Name),
    Bin2 = <<Bin/binary, Sep, NameBin/binary, $=, Value/binary>>,
    append_headers(Tail, $&, Bin2).

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec parse_test_() -> list().
parse_test_() ->
    [
     % samples from RFC
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = "atlanta.com"},
                   parse(<<"sip:alice@atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, password = <<"secretword">>,
                            host = "atlanta.com", params = [{transport, <<"tcp">>}]},
                   parse(<<"sip:alice:secretword@atlanta.com;transport=tcp">>)),
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"alice">>, host = "atlanta.com",
                            headers = [{subject, <<"project%20x">>}, {priority, <<"urgent">>}]},
                   parse(<<"sips:alice@atlanta.com?subject=project%20x&priority=urgent">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"+1-212-555-1212">>, password = <<"1234">>,
                            host = "gateway.com", params = [{user, <<"phone">>}]},
                   parse(<<"sip:+1-212-555-1212:1234@gateway.com;user=phone">>)),
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"1212">>, host = "gateway.com"},
                   parse(<<"sips:1212@gateway.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = {192, 0, 2, 4}},
                   parse(<<"sip:alice@192.0.2.4">>)),
     ?_assertEqual(#sip_uri{scheme = sip, host = "atlanta.com", params = [{method, <<"REGISTER">>}],
                            headers = [{to, <<"alice%40atlanta.com">>}]},
                   parse(<<"sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice;day=tuesday">>, host = "atlanta.com"},
                   parse(<<"sip:alice;day=tuesday@atlanta.com">>)),

     % extra test cases
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"alice">>, host = "atlanta.com",
                            params = [{maddr, <<"239.255.255.1">>}, {ttl, <<"15">>}]},
                   parse(<<"sips:alice@atlanta.com;maddr=239.255.255.1;ttl=15">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"j%40s0n">>, host = "atlanta.com"},
                   parse(<<"sip:j%40s0n@atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = {8193,3512,0,0,0,0,44577,44306}, port = 5060},
                   parse(<<"sip:alice@[2001:0db8:0000:0000:0000:0000:ae21:ad12]:5060">>))
     ].


-spec format_test_() -> list().
format_test_() ->
    [
     % samples from RFC
     ?_assertEqual(<<"sip:alice@atlanta.com">>,
                   format(#sip_uri{scheme = sip, user = <<"alice">>, host = "atlanta.com"})),
     ?_assertEqual(<<"sip:alice:secretword@atlanta.com;transport=tcp">>,
                   format(#sip_uri{scheme = sip, user = <<"alice">>, password = <<"secretword">>,
                                   host = "atlanta.com", params = [{transport, <<"tcp">>}]})),
     ?_assertEqual(<<"sips:alice@atlanta.com?subject=project%20x&priority=urgent">>,
                   format(#sip_uri{scheme = sips, user = <<"alice">>, host = "atlanta.com",
                                   headers = [{subject, <<"project%20x">>}, {priority, <<"urgent">>}]})),
     ?_assertEqual(<<"sip:+1-212-555-1212:1234@gateway.com;user=phone">>,
                   format(#sip_uri{scheme = sip, user = <<"+1-212-555-1212">>, password = <<"1234">>,
                                   host = "gateway.com", params = [{user, <<"phone">>}]})),
     ?_assertEqual(<<"sips:1212@gateway.com">>,
                   format(#sip_uri{scheme = sips, user = <<"1212">>, host = "gateway.com"})),
     ?_assertEqual(<<"sip:alice@192.0.2.4">>,
                   format(#sip_uri{scheme = sip, user = <<"alice">>, host = {192, 0, 2, 4}})),
     ?_assertEqual(<<"sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com">>,
                   format(#sip_uri{scheme = sip, host = "atlanta.com", params = [{method, <<"REGISTER">>}],
                                   headers = [{to, <<"alice%40atlanta.com">>}]})),
     ?_assertEqual(<<"sip:alice;day=tuesday@atlanta.com">>,
                   format(#sip_uri{scheme = sip, user = <<"alice;day=tuesday">>, host = "atlanta.com"})),

     % extra test cases
     ?_assertEqual(<<"sips:alice@atlanta.com;maddr=239.255.255.1;ttl=15">>,
                   format(#sip_uri{scheme = sips, user = <<"alice">>, host = "atlanta.com",
                                   params = [{maddr, <<"239.255.255.1">>}, {ttl, <<"15">>}]})),
     ?_assertEqual(<<"sip:j%40s0n@atlanta.com">>,
                   format(#sip_uri{scheme = sip, user = <<"j%40s0n">>, host = "atlanta.com"})),
     ?_assertEqual(<<"sip:alice@[2001:0db8:0000:0000:0000:0000:ae21:ad12]:5060">>,
                   format(#sip_uri{scheme = sip, user = <<"alice">>, host = {8193,3512,0,0,0,0,44577,44306}, port = 5060}))
     ].

-spec is_strict_router_test_() -> list().
is_strict_router_test_() ->
    [?_assertEqual(true, is_strict_router(sip_uri:parse(<<"sip:p3.middle.com">>))),
     ?_assertEqual(false, is_strict_router(sip_uri:parse(<<"sip:p2.example.com;lr">>)))
    ].

-endif.