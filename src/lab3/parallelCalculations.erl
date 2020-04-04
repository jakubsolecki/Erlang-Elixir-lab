%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2020 13:36
%%%-------------------------------------------------------------------
-module(parallelCalculations).
-author("Jakub Solecki").

%% API
-export([findMyParcelLocker/2, randomElems/3]).

findMyParcelLocker(PersonLocation, LockerLocations) ->
  Distances = [{{X, Y}, math:sqrt(math:pow(element(1, PersonLocation) - X, 2) +
    math:pow(element(2, PersonLocation) - Y, 2))} || {X, Y} <- LockerLocations],
  Fun =
    fun(A, B) ->
      element(2, A) =< element(2, B)
    end,
  {PersonLocation, element(1, lists:nth(1, lists:sort(Fun, Distances)))}.

randomElems(N, Min, Max) -> [{rand:uniform(Max - Min + 1) + Min - 1, rand:uniform(Max - Min + 1) + Min - 1}  ||
  _ <- lists:seq(1,  N)].

%% Call order
%% Lockers = parallelCalculations:randomElems(1000, 0, 10000).
%% Customers = parallelCalculations:randomElems(10000, 0, 10000).
%% Fun = fun(P) -> parallelCalculations:findMyParcelLocker(P, Lockers) end.
%% Result = lists:map(Fun, Customers).
