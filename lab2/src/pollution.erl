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
-export([createMonitor/0, addStation/3, addValues/5]).
%%-record(station, {name}).
%%-record(coords, {long, lat}).
%%-record()

%%createMonitor() -> [[], []].
%%
%%
%%addStation(Name, {Long, Lat}, [Stations, Readings]) ->
%%  case lists:member(Name, [StationName || {StationName, _} <- Stations]) of
%%    true ->
%%      io:format("Station ~s is already registered!~n", [Name]),
%%      stationAlreadyExistsError;
%%    false ->
%%      case lists:member({Long, Lat}, [{L, T} || {_, {L, T}} <- Stations]) of
%%        true ->
%%          io:format("Station at {~f, ~f} is already registered!~n", [Long, Lat]),
%%          stationAlreadyExistsError;
%%        false ->
%%          [[{Name, {Long, Lat}} | Stations], Readings]
%%      end
%%  end.
%%
%%
%%addValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, [Stations, Readings]) ->
%%  case lists:member(Station, [N || {N, _} <- Stations]) or lists:member(Station, [C || {_, C} <- Stations]) of
%%    false ->
%%      io:format("This reading is already registered!~n"),
%%      stationDoesNotExistError;
%%    true ->
%%      case lists:member({Station, {{Year, Month, Day}, Hour}, Type},
%%        [{StationIdentifier, {Date, H}, T} || {StationIdentifier, {Date, {H, _, _}}, T, _} <- Readings]) of
%%        true ->
%%          io:format("This reading is already registered!~n"),
%%          readingAlreadyExistsError;
%%        false ->
%%          [Stations, [{Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value} | Readings]]
%%      end
%%  end.
%%
%%
%%removeValue(Station, Date, Type, [Stations, Readings]) ->
%%  [Stations, [{S, D, T, V} || {S, D, T, V} <- Readings, {S, D, T} /= {Station, Date, Type}]].
%%
%%
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

addStation(Name, {Long, Lat}, [StationNames, StationCoords, Readings]) ->
  case {maps:find(Name, StationNames), maps:find({Long, Lat}, StationCoords)} of
    {error, error} -> [maps:put(Name, Name, StationNames), maps:put({Long, Lat}, {Long, Lat}, StationCoords), Readings];
    _ -> io:format("This station is already registered!~n"), error
  end.

addValues(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, [StationNames, StationCoords, Readings]) ->
  case {maps:find(Station, StationNames), maps:find(Station, StationCoords)} of
    {error, error} -> io:format("This station is not registered yet!~n"), error;
    _ -> case dict:is_key(Station, Readings) of
           true ->
             addNewValues(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, dict:fetch(Station, Readings),
               [StationNames, StationCoords, Readings]);
%%           TODO: check under alias of station (name -> {long, lat} or {long, lat} -> name)
           false -> [StationNames, StationCoords,
             dict:append(Station, {{{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value}, Readings)]
         end
  end.

addNewValues(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, List, [StationNames, StationCoords, Readings]) ->
%%  FIXME: exception error: no function clause matching :<
  case lists:member({{{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value }, [{{{Y, M, D}, {H, M, S}}, T, V} || {{{Y, M, D}, {H, M, S}}, T, V} <- List, {Y, M, D, H, T} == {Year, Month, Day, Hour, Type}]) of
    false -> [StationNames, StationCoords,
      dict:append(Station, {{{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value}, Readings)];
    true -> io:format("This reading is already registered!~n"), error
  end.
