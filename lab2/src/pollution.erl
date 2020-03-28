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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4]).


createMonitor() -> [[], []].


addStation(Name, {Long, Lat}, [Stations, Readings]) ->
  case lists:member(Name, [StationName || {StationName, _} <- Stations]) of
    true ->
      io:format("Station ~s is already registered!~n", [Name]),
      stationAlreadyExistsError;
    false ->
      case lists:member({Long, Lat}, [{L, T} || {_, {L, T}} <- Stations]) of
        true ->
          io:format("Station at {~f, ~f} is already registered!~n", [Long, Lat]),
          stationAlreadyExistsError;
        false ->
          [[{Name, {Long, Lat}} | Stations], Readings]
      end
  end.


addValue(Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value, [Stations, Readings]) ->
  case lists:member(Station, [N || {N, _} <- Stations]) or lists:member(Station, [C || {_, C} <- Stations]) of
    false ->
      io:format("This reading is already registered!~n"),
      stationDoesNotExistError;
    true ->
      case lists:member({Station, {{Year, Month, Day}, Hour}, Type},
        [{StationIdentifier, {Date, H}, T} || {StationIdentifier, {Date, {H, _, _}}, T, _} <- Readings]) of
        true ->
          io:format("This reading is already registered!~n"),
          readingAlreadyExistsError;
        false ->
          [Stations, [{Station, {{Year, Month, Day}, {Hour, Minute, Second}}, Type, Value} | Readings]]
      end
  end.


removeValue(Station, Date, Type, [Stations, Readings]) ->
  [Stations, [{S, D, T, V} || {S, D, T, V} <- Readings, {S, D, T} /= {Station, Date, Type}]].

