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
-export([createMonitor/0, addStation/3, addValues/5, removeValue/4, getOneValue/4]).


createMonitor() -> [maps:new(), dict:new()].


addStation(Name, {Long, Lat}, [Stations, Readings]) ->
  case {maps:find(Name, Stations), maps:find({Long, Lat}, Stations)} of
    {error, error} ->
      [maps:put(Name, {Name, {Long, Lat}}, maps:put({Long, Lat}, {Name, {Long, Lat}}, Stations)), Readings];
    _ -> io:format("This station is already registered!~n"), error
  end.

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
  case [{Y, Mon, D, H, T, V} || {{{Y, Mon, D}, H}, T, V} <- List,
    {Y, Mon, D, H, T} == {Year, Month, Day, Hour, Type}] of
    [] -> [Stations, dict:append(StationKey, {{{Year, Month, Day}, Hour}, Type, Value}, Readings)];
    _ -> io:format("This reading is already registered!~n"), error
  end.


removeValue(Station, {{Year, Month, Day}, Hour}, Type, [Stations, Readings]) ->
  Fun = fun(Val) ->
    [{{{Y, Mon, D}, H}, T, V} || {{{Y, Mon, D}, H}, T, V} <- Val,
      {Y, Mon, D, H, T} /= {Year, Month, Day, Hour, Type}]
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

