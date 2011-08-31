%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Utility functions to apply a list of functions to given state
%%%
%%% This module implements something similar to Error monad. If function
%%% returns `{stop, Result}', the whole computation is terminated and
%%% Result is returned. If `{next, Result}' is returned, the next function
%%% is invoked.
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
%%%----------------------------------------------------------------
-module(pipeline_m).

-export(['>>='/2, stop/1, next/1, return/1, process/2]).

-type monad(A) :: {next, A} | {stop, any()}.
-type valve(A, B) :: fun((A) -> monad(B)).

%% @doc Monad `bind' operator for pipeline monad.
%% @end
-spec '>>='(monad(A), valve(A, B)) -> monad(B).
'>>='({stop, _Result} = Stop, _Fun) -> Stop;
'>>='({next, Result},          Fun) -> Fun(Result).

%% @doc Stop computation operator
%% @end
-spec stop(T) -> {stop, T}.
stop(Result) -> {stop, Result}.

%% @doc Monad `return' operator.
%% @end
-spec return(T) -> {next, T}.
return(Result) -> {next, Result}.

%% @doc Alias for monad `return' operator.
%% @end
-spec next(T) -> {next, T}.
next(Result) -> return(Result).

%% @doc Pass value through the pipeline
%%
%% @end
-spec process(A, [valve(A, A)]) -> monad(A).
process(Acc0, Pipeline) ->
    lists:foldl(fun (ProcFun, Result) -> '>>='(Result, ProcFun) end, next(Acc0), Pipeline).
