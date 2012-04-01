%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP DNS resolution procedures
%%%
%%% TODO: IPv6 support
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3263</a> for details.
%%% @reference See <a href="http://tools.ietf.org/html/rfc2782">RFC 2782</a> for DNS SRV.
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(sip_resolve).

%% Exports

%% API
-export([server_resolve/1, client_resolve/1, resolve/1, request_destinations/1]).

%% Include files
-include("sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

-spec request_destinations(#sip_request{}) -> [#sip_destination{}].
%% @doc Resolve destinations to send for the given SIP request according to the 8.1.2
%% @end
request_destinations(Request) ->
    RequestURI = Request#sip_request.uri,
    URI =
        case sip_message:has_header(route, Request) of
            false -> RequestURI;
            true ->
                Route = sip_message:header_top_value(route, Request),
                % See RFC3261, 8.1.2
                % If first element in the route set is strict router,
                % use Request-URI
                case sip_uri:is_strict_router(Route#sip_hdr_address.uri) of
                    true -> RequestURI;
                    false -> Route
                end
        end,

    % if the Request-URI specifies a SIPS resource, consider URI to be SIPS as well
    URI2 =
        case sip_uri:is_sips(RequestURI) of
            true -> URI#sip_uri{scheme = sips};
            false -> URI
        end,
    sip_resolve:client_resolve(URI2).

%% @doc Generate list of destination for given URI.
%%
%% See <a href="http://tools.ietf.org/html/rfc3263#section-4">RFC 3263, 4 Client Usage</a>
%% @end
-spec client_resolve(term()) -> [#sip_destination{}].
client_resolve(#sip_uri{scheme = Scheme, port = Port} = URI) when is_record(URI, sip_uri) ->
    case lists:keyfind('maddr', 1, URI#sip_uri.params) of
        {_, MAddr} -> Target = sip_syntax:parse_ip_address(MAddr);
        false -> Target = URI#sip_uri.host
    end,

    % Check if we need TLS
    case Scheme of
        sips -> TLS = true, Params = [tls], DefTransport = tcp;
        sip -> TLS = false, Params = [], DefTransport = udp
    end,
    case lists:keyfind('transport', 1, URI#sip_uri.params) of
        {'transport', <<"tcp">>} -> Explicit = true, Transport = tcp;
        {'transport', <<"udp">>} -> Explicit = true, Transport = udp;
        {'transport', <<"sctp">>} -> Explicit = true, Transport = sctp;
        false -> Explicit = false, Transport = DefTransport
    end,

    if
        is_tuple(Target) ->
            % numeric IP is provided -- use it
            % use UDP for a SIP URI, and TCP for a SIPS URI
            Dest = #sip_destination{address = Target, port = Port, transport = Transport, params = Params},
            [Dest];
        is_integer(Port) ->
            % port is provided -- perform A or AAAA lookup
            % use UDP for a SIP URI, and TCP for a SIPS URI
            Dest = #sip_destination{port = Port, transport = Transport, params = Params},
            resolve_dest_a(Target, Dest);
        Explicit ->
            % Transport is provided explicitly, make SRV query
            Domain = service_proto(Transport, TLS) ++ Target,
            Dest = #sip_destination{transport = Transport, params = Params},
            resolve_dest_srv(Target, Domain, Dest);
        true ->
            % make NAPTR query
            select_bynaptr(Target, TLS)
     end.

%% @doc Select target service by making NAPTR query
%%
%% @end
select_bynaptr(Host, TLS) when is_boolean(TLS) ->
    % lookup NAPTR record for this domain, sort by order & preference
    List = lists:sort(inet_res:lookup(Host, in, naptr)),
    case List of
        [] ->
            % no NAPTR records -- try making SRV queries for all supported protocols
            if TLS -> Params = [tls]; not TLS -> Params = [] end,
            DomainTCP = service_proto(tcp, TLS) ++ Host,
            DestTCP = #sip_destination{transport = tcp, params = Params},
            case resolve_dest_srv(Host, DomainTCP, DestTCP) of
                [] ->
                    DomainUDP = service_proto(udp, TLS) ++ Host,
                    DestUDP = #sip_destination{transport = udp, params = Params},
                    resolve_dest_srv(Host, DomainUDP, DestUDP);
                Dests -> Dests
            end;
        _ ->
            % choose with lowest order, preference, which is supported by this implementation
            List2 = [{Service, Name} || {_Order, _Pref, _Flags, Service, RegExp, Name} <- List,
                                        is_supported(Service, TLS),
                                        RegExp =:= []],
            [{Service, Name} | _Rest] = List2,

            Dest2 = service_dest(Service),
            resolve_dest_srv(Host, Name, Dest2)
    end.

%% @doc Create `#sip_destination{}' for given NAPTR service
service_dest("sip+d2u") -> #sip_destination{transport = udp};
service_dest("sip+d2t") -> #sip_destination{transport = tcp};
service_dest("sip+d2s") -> #sip_destination{transport = sctp};
service_dest("sips+d2t") -> #sip_destination{transport = tcp, params = [tls]};
service_dest("sips+d2s") -> #sip_destination{transport = sctp, params = [tls]}.

%% @doc Check if we support given service/TLS requirement combination
%% @end
is_supported("sip+d2u", false) -> true;
is_supported("sip+d2t", false) -> true;
is_supported("sip+d2s", false) -> true;
is_supported("sips+d2t", _TLS) -> true;
is_supported("sips+d2s", _TLS) -> true;
is_supported(_Service, _TLS) -> false.

%% @doc Resolve hostname to IP address
%% @end
-spec resolve(string() | binary()) -> inet:ip_address().
resolve(Host) when is_binary(Host) -> resolve(binary_to_list(Host));
resolve(Host) when is_list(Host) ->
    {ok, Addr} = inet:getaddr(Host, inet), % FIXME: ipv6?
    Addr;
resolve(Addr) -> Addr. % must be an IPv4 or IPv6

%% @doc Generate list of destination for given top `Via:' value.
%%
%% See <a href="http://tools.ietf.org/html/rfc3263#section-5">RFC 3263, 5 Server Usage</a>
%% @end
-spec server_resolve(#sip_hdr_via{}) -> [#sip_destination{}].
server_resolve(Via) when is_record(Via, sip_hdr_via) ->
    case Via#sip_hdr_via.transport of
        tls -> Transport = tcp, Params = [tls], TLS = true;
        Transport -> Params = [], TLS = false
    end,
    Host = Via#sip_hdr_via.host,
    Port = Via#sip_hdr_via.port,
    if is_tuple(Host) ->
           % Host is IPv4/IPv6, return as is
           Dest = #sip_destination{address = Host, port = Port, transport = Transport, params = Params},
           [Dest];
       is_integer(Port) ->
            Dest = #sip_destination{port = Port, transport = Transport, params = Params},
            resolve_dest_a(Host, Dest); % Port is specified, make AAAA or A query
       true ->
           % otherwise, make SRV query
           Domain = service_proto(Transport, TLS) ++ Host,
           Dest = #sip_destination{transport = Transport, params = Params},
           resolve_dest_srv(Host, Domain, Dest)
    end.

%% @doc Determine Port and IP address by SRV queries
%% @end
resolve_dest_srv(Target, Domain, Dest) ->
    case lookup_srv(Domain) of
        [] ->
            % FIXME: hard-coded default port
            Port = 5060,
            [Res || Res <- resolve_dest_a(Target, Dest#sip_destination{port = Port})];
        SRVs ->
            [Res || {_Pri, _Weight, Port, Host} <- SRVs,
                    Res <- resolve_dest_a(Host, Dest#sip_destination{port = Port})]
    end.

%% @doc Lookup SRV records and sort them according to RFC 2782
%% @end
lookup_srv(Domain) ->
    SRVs = inet_res:lookup(Domain, in, srv),
    % First two elements are priority and weight, sort by them first
    sort_srvs(lists:sort(SRVs), []).

sort_srvs([], Acc) -> lists:reverse(Acc);
sort_srvs(SRVs, Acc0) ->
    [{First, _, _, _}|_] = SRVs, % Priority of first element
    {SamePri, Rest} = lists:splitwith(fun ({P, _, _, _}) -> P =:= First end, SRVs),
    Acc1 = order_byweight(SamePri, Acc0),
    sort_srvs(Rest, Acc1).

%% @doc Order the given list of SRV based on record's weight
%%
%% Selects item from the list based on the their weight, when prepends it
%% to the `Acc'. Note that items with greater weights will tend to be in
%% the end of the list, so we need to reverse it at the end.
%% @end
order_byweight(SRVs, Acc) ->
    % Calculate sum once, during the process we will simply substract from it
    % to get sum for the next step
    Sum = lists:foldl(fun({_, Weight, _, _}, Sum) -> Weight + Sum end, 0, SRVs),
    order_byweight(SRVs, Sum, Acc).

order_byweight([], _Sum, Acc) -> Acc;
order_byweight(List, Sum, Acc) ->
    % choose based on probability
    Random = crypto:rand_uniform(0, Sum + 1),
    Next = select_byweight(List, Random),

    % prepend to the results list, continue with remaining
    List2 = lists:delete(Next, List),
    Sum2 = Sum - element(2, Next),
    Acc2 = [Next | Acc],
    order_byweight(List2, Sum2, Acc2).

%% @doc Select first element from the list with running weight sum less or equal to the given value
%%
%% Select first element from the list with running weight sum less or equal to the given value.
%% Running weight sum is sum of all weights of record and all previous records. In the implementation
%% we actually substract the current weight from the parameter.
%% @end
select_byweight([{_, Weight, _, _} = SRV|_], RunWeight) when RunWeight =< Weight -> SRV;
select_byweight([{_, Weight, _, _}|Rest], RunWeight) -> select_byweight(Rest, RunWeight - Weight).


%% @doc Lookup A or AAAA records for given name and return list of `#sip_destination{}'
%% @end
resolve_dest_a(Host, Destination) when is_list(Host) ->
    {ok, Addrs} = inet:getaddrs(Host, inet), % FIXME: ipv6
    [Destination#sip_destination{address = Addr} || Addr <- Addrs].

%% @doc Determine proper SRV record based on given `#sip_destination{}' record.
%% @end
service_proto(udp, false) -> "_sip._udp.";
service_proto(tcp, false) -> "_sip._tcp.";
service_proto(tcp, true) -> "_sips._tcp.".
