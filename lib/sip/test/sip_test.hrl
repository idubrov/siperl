%%%----------------------------------------------------------------
%%% @author Ivan Dubrov <wfragg@gmail.com>
%%% @doc
%%% Helpful macroses for tests
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------

%% Includes
-ifndef(NO_TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% Macros
-define(TIMEOUT, 500).
-define(_fail(Msg), 
	.erlang:error({fail,
				  [{module, ?MODULE},
				   {line, ?LINE},
				   {msg, Msg}]})).
-define(fail(Msg), ?_fail((??Msg))).

%% Macros for functional testing of transaction layer
-define(assertReceive(Msg, Pattern, Timeout),
		(fun () -> receive 
					   Pattern -> ok
				   after Timeout ->  ?fail(Msg)
				   end
		 end)()).

-define(assertReceive(Msg, Pattern),
		?assertReceive(Msg, Pattern, ?TIMEOUT)).

-define(assertReceiveNot(Msg, Pattern, Timeout),
		(fun () -> receive
					   Pattern -> ?fail(Msg)
				   after Timeout -> ok
				   end
		 end)()).

-define(assertReceiveNot(Msg, Pattern),
		?assertReceiveNot(Msg, Pattern, ?TIMEOUT)).
