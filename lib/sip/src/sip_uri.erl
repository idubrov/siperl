%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc SIP URI parse utilities
%%%
%%% Implementation of parsing for supported URI schemes (`sip', `sips', `tel').
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-19">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_uri).

%% Exports

%% API
-export([parse/1]).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip.hrl").

-compile(export_all).

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
    {Host, Port, ParamsBin} = sip_binary:parse_host_port(Rest),
    {Params, HeadersBin} = parse_params(ParamsBin, []),
    Headers = parse_headers(HeadersBin),
    #sip_uri{scheme = Scheme,
             user = User,
             password = Password,
             host = Host,
             port = Port,
             headers = Headers,
             params = Params}.


%% Parse parameters lists
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
                {{sip_binary:binary_to_existing_atom(Name), Value}, R};
            {Name, Bin2} ->
                % no value
                {sip_binary:binary_to_existing_atom(Name), Bin2}
        end,
    parse_params(Rest, [Param|List]);
parse_params(Bin, List) ->
    {lists:reverse(List), sip_binary:trim_leading(Bin)}.


%% headers         =  "?" header *( "&" header )
%% header          =  hname "=" hvalue
%% hname           =  1*( hnv-unreserved / unreserved / escaped )
%% hvalue          =  *( hnv-unreserved / unreserved / escaped )
%% hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
parse_headers(<<>>) -> [];
parse_headers(<<$?, Bin/binary>>) ->
    Headers = [binary:split(Header, <<$=>>) || Header <- binary:split(Bin, <<$&>>)],
    [{sip_binary:binary_to_existing_atom(Name), Value} ||
     [Name, Value] <- Headers].

%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifndef(NO_TEST).

-spec parse_test_() -> list().
parse_test_() ->
    [
     % samples from RFC
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = <<"atlanta.com">>},
                   parse(<<"sip:alice@atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, password = <<"secretword">>,
                            host = <<"atlanta.com">>, params = [{transport, <<"tcp">>}]},
                   parse(<<"sip:alice:secretword@atlanta.com;transport=tcp">>)),
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"alice">>, host = <<"atlanta.com">>,
                            headers = [{subject, <<"project%20x">>}, {priority, <<"urgent">>}]},
                   parse(<<"sips:alice@atlanta.com?subject=project%20x&priority=urgent">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"+1-212-555-1212">>, password = <<"1234">>,
                            host = <<"gateway.com">>, params = [{user, <<"phone">>}]},
                   parse(<<"sip:+1-212-555-1212:1234@gateway.com;user=phone">>)),
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"1212">>, host = <<"gateway.com">>},
                   parse(<<"sips:1212@gateway.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = {192, 0, 2, 4}},
                   parse(<<"sip:alice@192.0.2.4">>)),
     ?_assertEqual(#sip_uri{scheme = sip, host = <<"atlanta.com">>, params = [{method, <<"REGISTER">>}],
                            headers = [{to, <<"alice%40atlanta.com">>}]},
                   parse(<<"sip:atlanta.com;method=REGISTER?to=alice%40atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice;day=tuesday">>, host = <<"atlanta.com">>},
                   parse(<<"sip:alice;day=tuesday@atlanta.com">>)),

     % extra test cases
     ?_assertEqual(#sip_uri{scheme = sips, user = <<"alice">>, host = <<"atlanta.com">>,
                            params = [{maddr, <<"239.255.255.1">>}, {ttl, <<"15">>}]},
                   parse(<<"sips:alice@atlanta.com;maddr=239.255.255.1;ttl=15">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"j%40s0n">>, host = <<"atlanta.com">>},
                   parse(<<"sip:j%40s0n@atlanta.com">>)),
     ?_assertEqual(#sip_uri{scheme = sip, user = <<"alice">>, host = {8193,3512,0,0,0,0,44577,44306}, port = 5060},
                   parse(<<"sip:alice@[2001:0db8:0000:0000:0000:0000:ae21:ad12]:5060">>))
     ].

-endif.