%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <wfragg@gmail.com>
%%% @doc SIP DNS resolution procedures
%%%
%%% TODO: IPv6 support
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3263">RFC 3263</a> for details.
%%% @reference See <a href="http://tools.ietf.org/html/rfc2782">RFC 2782</a> for DNS SRV.
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_resolve).

%% Exports

%% API
-export([destinations/1, resolve/1]).

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
client_resolve(URI) ->
    [#sip_destination{address = {127,0,0,1}, port = 15060, transport = udp},
     #sip_destination{address = {127,0,0,1}, port = 25060, transport = udp}
     ].

-spec resolve(binary()) -> inet:ip_address().
resolve(Bin) ->
    case sip_binary:parse_ip_address(Bin) of
        {ok, Addr} -> Addr;
        {error, invalid} ->
            {ok, Addr} = inet:getaddr(binary_to_list(Bin), inet),
            Addr
    end.

%% @doc Generate list of destination for given top `Via:' value.
%%
%% See <a href="http://tools.ietf.org/html/rfc3263#section-5">RFC 3263, 5 Server Usage</a>
%% @end
-spec destinations(#sip_hdr_via{}) -> [#sip_destination{}].
destinations(#sip_hdr_via{sent_by = {Host, Port}, transport = Transport}) ->
    case sip_binary:parse_ip_address(Host) of
        {ok, Addr} ->
            [#sip_destination{address = Addr, port = Port, transport = Transport}];
        {error, invalid} when Port =/= undefined ->
            % If, however, the sent-by field contained a domain name and a port
            % number, the server queries for A or AAAA records with that name.
            lookup_destinations(binary_to_list(Host), Port, Transport);
        {error, invalid} -> % domain name, no port
            % If, however, the sent-by field contained a domain name and no port,
            % the server queries for SRV records at that domain name
            SRVs = inet_res:lookup(transport_to_prefix(Transport) ++ binary_to_list(Host), in, srv),
            [Dest || {_Pri, _Weight, P, Target} <- sort_srvs(SRVs),
                     Dest <- lookup_destinations(Target, P, Transport)]
    end.

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


%% @doc
%% Lookup A or AAAA records for given name and return list of `#sip_destination{}'
%% @end
lookup_destinations(Target, Port, Transport) ->
    Addrs = inet_res:lookup(Target, in, a),
    [#sip_destination{address = Addr, port = Port, transport = Transport} || Addr <- Addrs].

transport_to_prefix(tcp) -> "_sip._tcp.";
transport_to_prefix(tls) -> "_sips._tcp.";
transport_to_prefix(udp) -> "_sip._udp.".
