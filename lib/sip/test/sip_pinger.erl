%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc Pinger UAC implementations. Sends OPTIONS request to the
%%% given destination.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_pinger).
-extends(sip_ua).

%% Exports

%% Include files
-include_lib("sip.hrl").
-include_lib("sip_test.hrl").

%% API
-export([start_link/0, ping/2]).

%% Server callbacks
-export([init/1]).
-export([handle_call/3, handle_response/3]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, {}, []).

ping(Pid, To) ->
    gen_server:call(Pid, {ping, To}, 100000).

%%-----------------------------------------------------------------
%% Server callbacks
%%-----------------------------------------------------------------
%% @private
init({}) ->
    sip_ua:init(?MODULE).

%% @private
handle_call({ping, To}, Client, State) ->
    From = sip_headers:address(<<"Mr. Pinger">>, <<"sip:pinger@127.0.0.1">>, []),
    Request = sip_ua:create_request('OPTIONS', To, From),
    ok = sip_ua:send_request(Request, Client),
    {noreply, State};
handle_call(Req, From, State) ->
    sip_ua:handle_call(Req, From, State).

-spec handle_response(term(), #sip_message{}, #sip_ua_state{}) -> any().
handle_response(Client, Response, State) ->
    gen_server:reply(Client, Response),
    {noreply, State}.
