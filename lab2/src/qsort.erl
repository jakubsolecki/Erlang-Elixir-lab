%%%-------------------------------------------------------------------
%%% @author Jakub
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2020 18:49
%%%-------------------------------------------------------------------
-module(qsort).
-author("Jakub").

%% API
-export([qs/1, lessThan/2, grtEqThan/2, randomElems/3, compareSpeeds/3]).


qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

%% Performance comparison

randomElems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min || _ <- lists:seq(1,  N)].

compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(Fun1, [List]),
  {T2, _} = timer:tc(Fun2, [List]),
  io:format("Sort1: ~p microseconds~nSort2: ~p microseconds~n", [T1, T2]).



