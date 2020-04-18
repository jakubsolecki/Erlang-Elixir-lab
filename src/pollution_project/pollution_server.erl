%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <AGH UST>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2020 13:18
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Jakub Solecki").

%% Server options
-export([start/0, stop/0, init/0]).
%% Interacting with pollution's functionality
-export([addStation/2, addValues/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/3,
  getSeasonalMean/4]).


start() -> register(pollServ, spawn(pollution_server, init, [])).

init() -> loop(pollution:createMonitor()).

stop() -> pollServ ! stop.

loop(Storage) ->
  receive
    {add_station, Name, Coords} ->
      UpdatedStorage = pollution:addStation(Name, Coords, Storage),
      case is_list(UpdatedStorage) of
        true -> loop(UpdatedStorage);
        false -> ret_val(UpdatedStorage), loop(Storage)
      end;
    {add_values, Station, Date, Type, Value} ->
      UpdatedStorage = pollution:addValues(Station, Date, Type, Value, Storage),
      case is_list(UpdatedStorage) of
        true -> loop(UpdatedStorage);
        false -> ret_val(UpdatedStorage), loop(Storage)
      end;
    {remove_value, Station ,Date, Type} ->
      UpdatedStorage = pollution:removeValue(Station, Date, Type, Storage),
      case is_list(UpdatedStorage) of
        true -> loop(UpdatedStorage);
        false -> ret_val(UpdatedStorage), loop(Storage)
      end;
    {get_one_value, Station ,Date, Type} ->
      Val = pollution:getOneValue(Station, Date, Type, Storage),
      case Val of
        {error, _} -> ret_val(Val), loop(Storage);
        _ ->
          io:format("Value of ~s from station ~w at ~w is ~w~n", [Type, Station, Date, Val]),
          loop(Storage)
      end;
    {get_station_mean, Station, Type} ->
      Mean = pollution:getStationMean(Station, Type, Storage),
      case Mean of
        {error, _} -> ret_val(Mean), loop(Storage);
        _ ->
          io:format("Mean of ~s from station ~w is ~w~n", [Type, Station, Mean]),
          loop(Storage)
      end;
    {get_daily_mean, Date, Type} ->
      Mean = pollution:getDailyMean(Date, Type, Storage),
      case Mean of
        {error, _} -> ret_val(Mean), loop(Storage);
        _ ->
          io:format("Daily mean of ~s is ~w~n", [Type, Mean]),
          loop(Storage)
      end;
    {get_hourly_mean, Station, Hour, Type} ->
      Mean = pollution:getHourlyMean(Station, Hour, Type, Storage),
      case Mean of
        {error, _} -> ret_val(Mean), loop(Storage);
        _ ->
          io:format("Mean of ~s at ~w:00 at ~w is ~w~n", [Type, Hour, Station, Mean]),
          loop(Storage)
      end;
    {get_seasonal_mean, Station, Start, End, Type} ->
      Mean = pollution:getSeasonalMean(Station, Start, End, Type, Storage),
      case Mean of
        {error, _} -> ret_val(Mean), loop(Storage);
        _ ->
          io:format("Mean of ~s at ~w for given interval of time is ~w~n", [Type, Station, Mean]),
          loop(Storage)
      end;
    stop -> ok
  end.

ret_val(Val) -> io:format("~w~n", [Val]).

addStation(Name, {Long, Lat}) ->
  pollServ ! {add_station, Name, {Long, Lat}}.

addValues(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value) ->
  pollServ ! {add_values, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value}.

removeValue(Station, {{Year, Month, Day}, Hour}, Type) ->
  pollServ ! {remove_value, Station, {{Year, Month, Day}, Hour}, Type}.

getOneValue(Station, {{Year, Month, Day}, Hour}, Type) ->
  pollServ ! {get_one_value, Station, {{Year, Month, Day}, Hour}, Type}.

getStationMean(Station, Type) ->
  pollServ ! {get_station_mean, Station ,Type}.

getDailyMean({Year, Month, Day}, Type) ->
  pollServ ! {get_daily_mean, {Year, Month, Day}, Type}.

getHourlyMean(Station, Hour, Type) ->
  pollServ ! {get_hourly_mean, Station, Hour, Type}.

getSeasonalMean(Station, {StartYear, StartMonth}, {EndYear, EndMoth}, Type) ->
  pollServ ! {get_seasonal_mean, Station, {StartYear, StartMonth}, {EndYear, EndMoth}, Type}.
