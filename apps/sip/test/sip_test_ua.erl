%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAC/UAS implementation tests
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_test_ua).

%% Exports
-export([start/0]).

%% Include files
-include("sip.hrl").
-include("sip_test.hrl").

%%-----------------------------------------------------------------
%% Functions
%%-----------------------------------------------------------------

%% @doc Performs most primitive UAC/UAS testing.
%%
%% This function starts two applications, `gproc' and `sip', starts
%% `sip_pingable' UAS implementation, `sip_pinger' UAC implementation
%% and makes UAC to send `OPTIONS' request to the `UAS'. The result
%% is printed.
%%
%% Run the scenario by specifying `-run' option to the `erl'.
%% ```
%% '''
%% @end
-spec start() -> ok.
start() ->
    application:start(gproc),
    application:start(sip),
    sip_pingable:start_link(),
    {ok, Pinger} = sip_pinger:start_link(),
    {ok, Response} = sip_pinger:ping(Pinger, sip_headers:address(<<>>, <<"sip:127.0.0.1">>, [])),
    io:format("Received response:~n~p~n", [Response]).
