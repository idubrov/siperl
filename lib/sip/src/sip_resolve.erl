%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc SIP DNS resolution procedures
%%%
%%% TODO: IPv6 support
%%% FIXME: should have already parsed adresses...
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3263</a> for details.
%%% @reference See <a href="http://tools.ietf.org/html/rfc2782">RFC 2782</a> for DNS SRV.
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_resolve).

%% Exports

%% API
-export([server_resolve/1, client_resolve/1, resolve/1]).

%% Include files
-include_lib("sip_common.hrl").
-include_lib("sip.hrl").

-compile(export_all).

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------

%% @doc Generate list of destination for given URI.
%%
%% See <a href="http://tools.ietf.org/html/rfc3263#section-4">RFC 3263, 4 Client Usage</a>
%% @end
-spec client_resolve(term()) -> [#sip_destination{}].
client_resolve(#sip_uri{scheme = Scheme, port = Port} = URI) when is_record(URI, sip_uri) ->
    Target =
       case lists:keyfind('maddr', 1, URI#sip_uri.params) of
           {_, MAddr} ->
               sip_binary:parse_ip_address(MAddr);
           false ->
               URI#sip_uri.host
       end,
    
    case lists:keyfind('transport', 1, URI#sip_uri.params) of
        {'transport', Transport} ->
            TargetStr = binary_to_list(Target),
            lookup_srvs(transport_to_prefix(Transport) ++ TargetStr, tcp);
        false ->
            
            if is_tuple(Target) ->
                   % explicit IP address specified
                   Transport = transport(Scheme),
                   [#sip_destination{address = Target, port = URI#sip_uri.port, transport = Transport}];
               URI#sip_uri.port =/= undefined ->
                   % explicit port is provided, make A or AAAA request
                   Transport = transport(Scheme),
                   lookup_destinations(binary_to_list(Target), Port, Transport);
               true ->
                   % no transport protocol, no port, no explicit numeric IP
                   % send NAPTR request
                   select_bynaptr(Target, Scheme)
            end
    end.

%% @doc Select target service by making NAPTR query
%%
%% @end
select_bynaptr(Target, Scheme) ->
    List = inet_res:lookup(binary_to_list(Target), in, naptr),
    % sort by order & preference
    case lists:sort(List) of
        [] ->
            % no NAPTR records -- try making SRV queries
            % FIXME: sips URIs!
            TargetStr = binary_to_list(Target),
            case lookup_srvs("_sip._tcp." ++ TargetStr, tcp) of
                [] -> lookup_srvs("_sip._udp." ++ TargetStr, udp);
                Dests -> Dests
            end;
        Sorted ->
            % choose with lowest order, preference, which is supported by this implementation
            List2 = [{Service, Name} || {_Order, _Pref, _Flags, Service, RegExp, Name} <- Sorted,
                                        supported(Service, Scheme),
                                        RegExp =:= []],
            [{Service, Name} | _Rest] = List2,
            Transport = transport(Service),
            lookup_srvs(Name, Transport)
    end.

%% @doc Return default transport for given URI scheme or NAPTR service
%% @end
transport(sip) -> udp;
transport(sips) -> tcp;
transport("sip+d2u") -> udp;
transport("sip+d2t") -> tcp;
transport("sip+d2s") -> sctp;
transport("sips+d2u") -> udp;
transport("sips+d2t") -> tcp;
transport("sips+d2s") -> stp.

%% @doc Check if we support given service/URI scheme combination
%% @end
supported("sip+d2u", sip) -> true; % only for SIP URIs
supported("sip+d2t", sip) -> true; % only for SIP URIs
supported(_Service, _Scheme) -> false. % FIXME: no TLS for now!

-spec resolve(binary()) -> inet:ip_address().
resolve(Bin) when is_binary(Bin) ->
    {ok, Addr} = inet:getaddr(binary_to_list(Bin), inet),
    Addr;
resolve(Addr) -> Addr. % must be an IPv4 or IPv6

%% @doc Generate list of destination for given top `Via:' value.
%%
%% See <a href="http://tools.ietf.org/html/rfc3263#section-5">RFC 3263, 5 Server Usage</a>
%% @end
-spec server_resolve(#sip_hdr_via{}) -> [#sip_destination{}].
server_resolve(#sip_hdr_via{host = Host, port = Port, transport = Transport}) ->
    case Host of
        {_, _, _, _} ->
            % IPv4
            [#sip_destination{address = Host, port = Port, transport = Transport}];
        {_, _, _, _, _, _, _, _} ->
            % IPv6
            [#sip_destination{address = Host, port = Port, transport = Transport}];
        _HostName when Port =/= undefined ->
            % If, however, the sent-by field contained a domain name and a port
            % number, the server queries for A or AAAA records with that name.
            lookup_destinations(binary_to_list(Host), Port, Transport);
        _HostName -> % domain name, no port
            % If, however, the sent-by field contained a domain name and no port,
            % the server queries for SRV records at that domain name
            lookup_srvs(transport_to_prefix(Transport) ++ binary_to_list(Host), Transport)
    end.

lookup_srvs(Host, Transport) ->
    SRVs = inet_res:lookup(Host, in, srv),
    [Dest || {_Pri, _Weight, P, Target} <- sort_srvs(SRVs),
             Dest <- lookup_destinations(Target, P, Transport)].

% FIXME: sort according to RFC 2782
sort_srvs(SRVs) ->
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
lookup_destinations(Target, Port, Transport) ->
    Addrs = inet_res:lookup(Target, in, a),
    [#sip_destination{address = Addr, port = Port, transport = Transport} || Addr <- Addrs].

transport_to_prefix(tcp) -> "_sip._tcp.";
transport_to_prefix(tls) -> "_sips._tcp.";
transport_to_prefix(udp) -> "_sip._udp.".
