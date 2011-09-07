%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Pipeline monad
%%%
%%% Basically, this monad is equivalent to `error_m' monad provided
%%% by Erlando. The only difference is more meaningful names.
%%% `{next, T}', which is equivalent of `{ok, T}' is to invoke next
%%% operation. `{stop, Result}', which is equivalent of `{error, Result}'
%%% is to stop computation and return result.
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(pipeline_m).

-export(['>>='/2, return/1, fail/1, next/1, stop/1]).

-type monad(A) :: {next, A} | {stop, any()}.
-type valve(A, B) :: fun((A) -> monad(B)).

%% @doc Monad `bind' operator for pipeline monad.
%% @end
-spec '>>='(monad(A), valve(A, B)) -> monad(B).
'>>='({stop, _Result} = Stop, _Fun) -> Stop;
'>>='({next, Result},          Fun) -> Fun(Result).

%% @doc Monad `return' operator.
%% @end
-spec return(T) -> {next, T}.
return(Result) -> {next, Result}.

%% @doc Alias for monad `return' operator.
%% @end
-spec next(T) -> {next, T}.
next(Result) -> return(Result).

%% @doc Monad `fail' operator
%% @end
-spec fail(T) -> {stop, T}.
fail(Result) -> {stop, Result}.

%% @doc Alias for monad `fail' operator
%% @end
-spec stop(T) -> {stop, T}.
stop(Result) -> fail(Result).
