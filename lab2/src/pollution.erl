%%%-------------------------------------------------------------------
%%% @author Mithrian812
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mar 2020 18:53
%%%-------------------------------------------------------------------
-module(pollution).
-author("Mithrian812").

%% API
-export([createMonitor/0, addStation/3, addValues/5, removeValue/4]).


%%getOneValue(Station, {{Year, Month, Day}, Hour}, Type, [Stations, Readings]) ->
%%  case is_tuple(Station) of
%%    true -> getOneValue(Type, {{Year, Month, Day}, Hour}, lists:keyfind(Station, 2, Stations), [Stations, Readings]);
%%    false -> getOneValue(Type, {{Year, Month, Day}, Hour}, lists:keyfind(Station, 1, Stations), [Stations, Readings])
%%  end;
%%getOneValue(_, _, false, [_, _]) ->
%%  io:format("This station is not registered"),
%%  noStationError;
%%getOneValue(Type, {{Year, Month, Day}, Hour}, {Name, {Long, Lat}}, [Stations, Readings]) ->
%%  case [{N, {{Y, M, D}, {H, M, S}}, T, V} || {N, {{Y, M, D}, {H, M, S}}, T, V} <- Readings,
%%    {N, {{Y, M, D}, H}, T} == {Name, {{Year, Month, Day}, Hour}, Type}] of
%%          [Success] -> Success;
%%          [] -> case [{{L, T}, {{Y, M, D}, {H, M, S}}, T, V} || {{L, T}, {{Y, M, D}, {H, M, S}}, T, V} <- Readings,
%%            {{L, T}, {{Y, M, D}, H}, T} == {Name, {{Year, Month, Day}, Hour}, Type}] of
%%                  [Success] -> Success;
%%                  [] -> io:format("Nothing was found"), noReadingError
%%                end
%%  end.


%% with dict and maps

createMonitor() -> [maps:new(), maps:new(), dict:new()].


%%TODO: one map for stations
addStation(Name, {Long, Lat}, [StationNames, StationCoords, Readings]) ->
  case {maps:find(Name, StationNames), maps:find({Long, Lat}, StationCoords)} of
    {error, error} -> [maps:put(Name, {Name, {Long, Lat}}, StationNames), maps:put({Long, Lat},
      {Name, {Long, Lat}}, StationCoords), Readings];
    _ -> io:format("This station is already registered!~n"), error
  end.


addValues(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, [StationNames, StationCoords, Readings]) ->
  case {maps:find(Station, StationNames), maps:find(Station, StationCoords)} of
    {error, error} -> io:format("This station is not registered yet!~n"), error;
    {{ok, {Name, Coords}}, error} ->
      checkBeforeAdd(Name, Coords, Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value,
        [StationNames, StationCoords, Readings]);
    {error, {ok, {Name, Coords}}} ->
      checkBeforeAdd(Name, Coords, Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value,
        [StationNames, StationCoords, Readings])
  end.

checkBeforeAdd(Name, Coords, Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value,
    [StationNames, StationCoords, Readings]) ->
  case dict:is_key(Station, Readings) of
    true ->
     addNewValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, dict:fetch(Station, Readings),
       [StationNames, StationCoords, Readings]);
    false ->
      case dict:is_key(Name, Readings) of
        true ->
          addNewValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, dict:fetch(Name, Readings),
            [StationNames, StationCoords, Readings]);
        false ->
          case dict:is_key(Coords, Readings) of
            true ->
              addNewValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value,
                dict:fetch(Coords, Readings), [StationNames, StationCoords, Readings]);
            false ->
              [StationNames, StationCoords,
                dict:append(Station, {{{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value}, Readings)]
          end
      end
  end.

addNewValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, List,
    [StationNames, StationCoords, Readings]) ->
  case [{Y, Mon, D, H, Min, S, T, V} || {{{Y, Mon, D}, {H, Min, S}}, T, V} <- List,
    {Y, Mon, D, H, T} == {Year, Month, Day, Hour, Type}] of
    [] -> [StationNames, StationCoords,
      dict:append(Station, {{{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value}, Readings)];
    _ -> io:format("This reading is already registered!~n"), error
  end.


removeValue(Station, {{Year, Month, Day}, Hour}, Type, [StationNames, StationCoords, Readings]) ->
  Fun = fun(Val) ->
    [{{{Y, Mon, D}, {H, Min, S}}, T, V} || {{{Y, Mon, D}, {H, Min, S}}, T, V} <- Val,
      {Y, Mon, D, H, T} /= {Year, Month, Day, Hour, Type}]
        end,
  case {maps:find(Station, StationNames), maps:find(Station, StationCoords)} of
    {error, error} -> io:format("This station is not registered yet!~n"), error;
    {{ok, {Name, Coords}}, error} ->
      removeReading(Name, Coords, Station, Fun, [StationNames, StationCoords, Readings]);
    {error, {ok, {Name, Coords}}} ->
      removeReading(Name, Coords, Station, Fun, [StationNames, StationCoords, Readings])
  end.

removeReading(Name, Coords, Station, Fun, [StationNames, StationCoords, Readings]) ->
  case dict:is_key(Station, Readings) of
    true ->
      [StationNames, StationCoords, dict:update(Station, Fun, Readings)];
    false ->
      case dict:is_key(Name, Readings) of
        true ->
          [StationNames, StationCoords, dict:update(Name, Fun, Readings)];
        false ->
          case dict:is_key(Coords, Readings) of
            true ->
              [StationNames, StationCoords, dict:update(Coords, Fun, Readings)];
            false -> io:format("The key was not found in dictionary. How could this happen?~n"), superError
          end
      end
  end.


