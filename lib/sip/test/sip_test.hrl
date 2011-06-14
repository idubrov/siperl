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
-define(fail(Msg), 
	.erlang:error({fail,
				  [{module, ?MODULE},
				   {line, ?LINE},
				   {msg, (??Msg)}]})).

