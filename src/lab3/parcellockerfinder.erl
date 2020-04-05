%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. kwi 2020 13:36
%%%-------------------------------------------------------------------
-module(parcellockerfinder).
-author("Jakub Solecki").

%% API
-export([findMyParcelLocker/2, findMyParcelLocker/3, randomElems/3, start/3, server/3, timeMeasure/0, len/1, splitList/3,
  start4cores/4, findMyParcelLockerSerial/3]).


randomElems(N, Min, Max) -> [{rand:uniform(Max - Min + 1) + Min - 1, rand:uniform(Max - Min + 1) + Min - 1}  ||
  _ <- lists:seq(1,  N)].


%% Standard approach

%% Call order
%% Lockers = parcellockerfinder:randomElems(1000, 0, 10000).
%% Customers = parcellockerfinder:randomElems(10000, 0, 10000).
%% Fun = fun(P) -> parcellockerfinder:findMyParcelLocker(P, Lockers) end.
%% timer:tc(lists, map, [Fun, Customers]).

findMyParcelLocker(PersonLocation, LockerLocations) ->
  Distances = [{{X, Y}, math:sqrt(math:pow(element(1, PersonLocation) - X, 2) +
    math:pow(element(2, PersonLocation) - Y, 2))} || {X, Y} <- LockerLocations],
  Fun =
    fun(A, B) ->
      element(2, A) =< element(2, B)
    end,
  {PersonLocation, element(1, lists:nth(1, lists:sort(Fun, Distances)))}.



%% Using separate process for each person

%% Call order
%% Lockers = parcellockerfinder:randomElems(1000, 0, 10000).
%% Customers = parcellockerfinder:randomElems(10000, 0, 10000).
%% parcellockerfinder:start(Customers, Lockers, parcellockerfinder:len(Customers)).

findMyParcelLocker(PPID, PersonLocation, LockerLocations) ->
  Distances = [{{X, Y}, math:sqrt(math:pow(element(1, PersonLocation) - X, 2) +
    math:pow(element(2, PersonLocation) - Y, 2))} || {X, Y} <- LockerLocations],
  Fun =
    fun(A, B) ->
      element(2, A) =< element(2, B)
    end,
  PPID ! {PersonLocation, element(1, lists:nth(1, lists:sort(Fun, Distances)))}.

start(PeopleLocations, LockerLocations, End) ->
  MPID = spawn(parcellockerfinder, timeMeasure, []),
  register(myTimer, MPID),
  myTimer ! {t1, erlang:timestamp()},
  PPID = spawn(parcellockerfinder, server, [0, End, []]),
%%  register(server, PPID),
  spawn(fun() -> loop(PPID, PeopleLocations, LockerLocations) end).

server(Count, End, List) ->
  receive
    stop -> io:format("~w ~w~n", [len(List), List]);
    Res ->
      case Count+1 == End of
        false -> server(Count+1, End, [Res | List]);
        true ->
          io:format("~w ~w~n", [len([Res | List]), [Res | List]]),
          myTimer ! {t2, erlang:timestamp()}
      end
  end.

loop(_, [], _) -> ok;
loop(PPID, [H | PeopleLocations], LockerLocations) ->
  spawn(fun() -> findMyParcelLocker(PPID, H, LockerLocations) end),
  loop(PPID, PeopleLocations, LockerLocations).

len(List) -> len(List, 0).
len([], N) -> N;
len([_ | Rest], N) -> len(Rest, N+1).

timeMeasure() ->
  receive
    {t1, T} -> Time1 = T, timeMeasure(Time1)
  end.
timeMeasure(Time1) ->
  receive
    {t2, T} -> io:format("Time (microseconds): ~w~n", [timer:now_diff(T, Time1)])
  end.



%% 4 cores version

%% Call order
%% Lockers = parcellockerfinder:randomElems(1000, 0, 10000).
%% Customers = parcellockerfinder:randomElems(10000, 0, 10000).
%% parcellockerfinder:start4cores(Customers, Lockers, 4, 2500).


findMyParcelLockerSerial(PPID, PeopleLocations, LockerLocations) ->
  Fun = fun(P) -> parcellockerfinder:findMyParcelLocker(P, LockerLocations) end,
  PPID ! lists:map(Fun, PeopleLocations).

start4cores(PeopleLocations, LockerLocations, End, Mod) ->
  MPID = spawn(parcellockerfinder, timeMeasure, []),
  register(myTimer, MPID),
  myTimer ! {t1, erlang:timestamp()},
  PPID = spawn(parcellockerfinder, server, [0, End, []]),
  Fun = fun(X) -> parcellockerfinder:findMyParcelLockerSerial(PPID, X, LockerLocations) end,
  splitList(PeopleLocations, Mod, Fun).

splitList(List, Mod, Fun) ->
  splitList(List, Mod, [], 0, Fun).
splitList([], _, Acc, _, Fun) ->
  spawn(fun() -> Fun(Acc) end);
splitList([H | Tail], Mod, Acc, Count, Fun) ->
  case {Count > 0, Count rem Mod == 0} of
    {true, true} ->
      spawn(fun() -> Fun(Acc) end),
      splitList(Tail, Mod, [H], Count+1, Fun);
    _ -> splitList(Tail, Mod, [H | Acc], Count+1, Fun)
  end.
