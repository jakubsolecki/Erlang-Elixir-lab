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
-export([createMonitor/0, addStation/3, addValues/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).


%% Create list containing map for storing stations and dictionary for storing reading from those stations
createMonitor() -> [maps:new(), dict:new()].


%% Stations are stored in map two times: the tuple containing all data about each station ({name, {longitude, latitude}})
%% is bound to key with value of station's name and second time under the key with value of station's co-ordinates.
%% It makes searching operations less complicated.
addStation(Name, {Long, Lat}, [Stations, Readings]) ->
  case {maps:find(Name, Stations), maps:find({Long, Lat}, Stations)} of
    {error, error} ->
      [maps:put(Name, {Name, {Long, Lat}}, maps:put({Long, Lat}, {Name, {Long, Lat}}, Stations)), Readings];
    _ -> io:format("This station is already registered!~n"), error
  end.


%% Key, which readings are stored under, is a complete tuple taken from map of stations. Then there are two possibilities:
%% 1) Key exists in dictionary -> there are already some readings -> there's need to check them so we won't have duplicates.
%%    This case is handled in an assistant function addNewValue/6.
%% 2) Key does not exist yet in dictionary -> we can proceed and simply add it with value of reading.
%% Station - name or co-ordinates of the station, tuple with datetime is a standard form returned by calendar:local_time().
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

%% Function is called from exported addValues/5. Argument "List" is a list of values stored under key checked in addValues/5.
%% Using list comprehensions we determine whether reading (yet to be added) already exists; if it does, we communicate
%% that and end with an error; if it does not, we add it to dictionary.
%% StationKey is a tuple from map of stations.
addNewValue(StationKey, {{Year, Month, Day}, Hour}, Type, Value, List, [Stations, Readings]) ->
  case [{Y, Mon, D, H, T, V} || {{{Y, Mon, D}, H}, T, V} <- List,
    {Y, Mon, D, H, T} == {Year, Month, Day, Hour, Type}] of
    [] -> [Stations, dict:append(StationKey, {{{Year, Month, Day}, Hour}, Type, Value}, Readings)];
    _ -> io:format("This reading is already registered!~n"), error
  end.


%% Value is removed from dictionary using dict:update/3: list of values under the key is filtered and copied without
%% given value.
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


%% Function returns value of specified type from specified station and date. It is done using simple list comprehensions
%% on list of the values under key (station). Unlike the previous functions, it returns found value, not updated list with
%% map and dictionary.
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
%% This function also return only calculated value.
getStationMean(Station, Type, [Stations, Readings]) ->
  case maps:find(Station, Stations) of
    error -> io:format("This station is not registered yet!~n"), error;
    {ok, StationKey} ->
      case [V || {{{_, _, _}, _}, T, V} <- dict:fetch(StationKey, Readings), T == Type] of
        [] -> 0;
        List ->
          {Sum, Count} = count(List),
          Sum/Count
      end
  end.


%% Assistant functions to calculate length of a list and sum of all its values.
count(List) -> count(List, {0, 0}).

count([], {Sum, Count}) -> {Sum, Count};
count([H | Rest], {Sum, Count}) -> count(Rest, {Sum + H, Count+1}).


%% Calculates daily mean of specified type value for all stations.
%% This function also return only calculated value.
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

