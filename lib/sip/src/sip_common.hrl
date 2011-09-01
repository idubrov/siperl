%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
%-define(NO_TEST, true).

%%-----------------------------------------------------------------
%% Includes
%%-----------------------------------------------------------------
-ifndef(NO_TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%%-----------------------------------------------------------------
%% Common supervisor children specs
%%-----------------------------------------------------------------
-define(DEF_SHUTDOWN, 2000).
-define(SPEC(Name, Module, Type, Args), {Name, {Module, start_link, Args},
                                         permanent, ?DEF_SHUTDOWN, Type, [Module]}).
-define(SPEC(Module, Type, Args),     ?SPEC(Module, Module, Type, Args)).
-define(SPEC(Module, Type),         ?SPEC(Module, Type, [])).
-define(WORKER(Module),             ?SPEC(Module, worker, [])).
-define(WORKER(Module, Args),         ?SPEC(Module, worker, Args)).
