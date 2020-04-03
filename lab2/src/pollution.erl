%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 18:53
%%%-------------------------------------------------------------------
-module(pollution).
-author("Jakub Solecki").

%% API
-export([createMonitor/0, addStation/3, addValues/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
  getHourlyMean/4, getSeasonalMean/5]).


%% Creates list containing stations and readings
createMonitor() -> [maps:new(), dict:new()].


%% Register station (doubled, in order to simplify searching)
addStation(Name, {Long, Lat}, [Stations, Readings]) ->
  case {maps:find(Name, Stations), maps:find({Long, Lat}, Stations)} of
    {error, error} ->
      [maps:put(Name, {Name, {Long, Lat}}, maps:put({Long, Lat}, {Name, {Long, Lat}}, Stations)), Readings];
    _ -> io:format("This station is already registered!~n"), error
  end.


%% Add reading (from one of the registered stations)
addValues(Station, {{Year, Month, Day}, {Hour, _, _}}, Type, Value, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This station is not registered yet!~n"), error;
    {ok, StationKey} ->
      case dict:is_key(StationKey, Readings) of
        true ->
          addNewValue(StationKey, {{Year, Month, Day}, Hour}, Type, Value,
            dict:fetch(StationKey, Readings), [Stations, Readings]);
        false ->
          [Stations, dict:append(StationKey, {{{Year, Month, Day}, Hour}, Type, Value}, Readings)]
      end
  end.

addNewValue(StationKey, {{Year, Month, Day}, Hour}, Type, Value, List, [Stations, Readings]) ->
  case [{Y, M, D, H, T, V} || {{{Y, M, D}, H}, T, V} <- List,
    {Y, M, D, H, T} == {Year, Month, Day, Hour, Type}] of
    [] -> [Stations, dict:append(StationKey, {{{Year, Month, Day}, Hour}, Type, Value}, Readings)];
    _ -> io:format("This reading is already registered!~n"), error
  end.


%% Remove reading (from one of the registered stations)
removeValue(Station, {{Year, Month, Day}, Hour}, Type, [Stations, Readings]) ->
  Fun = fun(Val) ->
    [{{{Y, M, D}, H}, T, V} || {{{Y, M, D}, H}, T, V} <- Val,
      {Y, M, D, H, T} /= {Year, Month, Day, Hour, Type}]
        end,
  case maps:find(Station, Stations) of
    error -> io:format("This station is not registered yet!~n"), error;
    {ok, StationKey} ->
      case dict:is_key(StationKey, Readings) of
        true ->
          [Stations, dict:update(StationKey, Fun, Readings)];
        false -> io:format("This station does not have any readings yet!~n")
      end
  end.


%% Return value of reading containing provided: type, station and date.
getOneValue(Station, {{Year, Month, Day}, Hour}, Type, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This station is not registered yet!~n"), error;
    {ok, StationKey} ->
      case dict:is_key(StationKey, Readings) of
        false -> io:format("This station does not have any readings yet!~n");
        true ->
          case [{{{Y, Mon, D}, H}, T, V} || {{{Y, Mon, D}, H}, T, V} <- dict:fetch(StationKey, Readings),
            {Y, Mon, D, H, T} == {Year, Month, Day, Hour, Type}] of
            [Result] -> Result;
            [] -> io:format("There's no such a reading!~n"), error
          end
      end
  end.


%% Functions calculates mean of values of specified type, from given station (all its readings in history).
getStationMean(Station, Type, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This station is not registered yet!~n"), error;
    {ok, StationKey} ->
      case [V || {_, T, V} <- dict:fetch(StationKey, Readings), T == Type] of
        [] -> 0;
        List ->
          {Sum, Count} = count(List),
          Sum/Count
      end
  end.


%% Calculate length of a list and sum of all its values.
count(List) -> count(List, {0, 0}).

count([], {Sum, Count}) -> {Sum, Count};
count([H | Rest], {Sum, Count}) -> count(Rest, {Sum + H, Count+1}).


%% Calculate daily mean of specified type value for all stations.
getDailyMean({Year, Month, Day}, Type, [Stations, Readings]) ->
  Fun = fun(Key, ListOfVals, {Sum, Count}) ->
    {S, C} = count([V || {{{Y, M, D}, _}, T, V} <- ListOfVals, {Y, M, D, T} == {Year, Month, Day, Type}]),
    {Sum + S, Count + C}
        end,
  {Sres, Cres} = dict:fold(Fun, {0, 0}, Readings),
  case {Sres, Cres} == {0, 0} of
    true -> 0;
    false -> Sres/Cres
  end.


%% Returns mean of all values of specified types on given station at provided hour.
getHourlyMean(Station, Hour, Type, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This satation is not registered yet!~n"), error;
    {ok, StationKey} ->
      case [V || {{_, H}, T, V} <- dict:fetch(StationKey, Readings), {T, H} == {Type, Hour}] of
        [] -> 0;
        List ->
          {Sum, Count} = count(List),
          Sum/Count
      end
  end.


%% Returns mean of values of specified type from given time interval.
getSeasonalMean(Station, {StartYear, StartMonth}, {EndYear, EndMoth}, Type, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This satation is not registered yet!~n"), error;
    {ok, StationKey} ->
      case [V || {{{Y, M, _}, H}, T, V} <- dict:fetch(StationKey, Readings),
        Y >= StartYear, M >= StartMonth, Y =< EndYear, M =<  EndMoth, T == Type] of
        [] -> 0;
        List ->
          {Sum, Count} = count(List),
          Sum/Count
      end
  end.
