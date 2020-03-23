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
-export([qs/1, lessThan/2, grtEqThan/2, randomElems/3, compareSpeeds/3, map/2, filter/2, sumDigits/1]).


% Quicksort
qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

randomElems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1,  N)].

compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(Fun1, [List]),
  {T2, _} = timer:tc(Fun2, [List]),
  io:format("Sort1: ~p microseconds~nSort2: ~p microseconds~n", [T1, T2]).


%% Fun
map(_, []) -> [];
map(F, [H | T]) -> [F(H) | qsort:map(F, T)].

filter(_, []) -> [];
filter(P, L) -> [Y || Y <- L, P(Y)].

sumDigits(X) ->
  L = [list_to_integer([Char]) || Char <- integer_to_list(X)],
  lists:foldl(fun(Y, Acc) -> Y + Acc end, 0, L).


%% From numbers in range [1, 1 000 000] pick those, which sum if digits is divisible by 3.
%% (The two lines below ought to be run in Eshell in given order)
%% L = qsort:randomElems(1000000, 1, 1000000).
%% lists:filter(fun(X) -> case qsort:sumDigits(X) rem 3 == 0 of true -> true; (_) -> false end end, L).
