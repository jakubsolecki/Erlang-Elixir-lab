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

%% Managing the server
start() -> register(pollServ, spawn(pollution_server, init, [])).

init() -> loop(pollution:createMonitor()).

stop() -> pollServ ! stop.

%% Main sever loop
loop(Storage) ->
  receive
    {PID, {add_station, Name, Coords}} ->
      UpdatedStorage = pollution:addStation(Name, Coords, Storage),
      case is_list(UpdatedStorage) of
        true ->
          PID ! {reply, ok},
          loop(UpdatedStorage);
        false ->
          PID ! {reply, UpdatedStorage},
          loop(Storage)
      end;
    {PID, {add_values, Station, Date, Type, Value}} ->
      UpdatedStorage = pollution:addValues(Station, Date, Type, Value, Storage),
      case is_list(UpdatedStorage) of
        true ->
          PID ! {reply, ok},
          loop(UpdatedStorage);
        false ->
          PID ! {reply, UpdatedStorage},
          loop(Storage)
      end;
    {PID, {remove_value, Station ,Date, Type}} ->
      UpdatedStorage = pollution:removeValue(Station, Date, Type, Storage),
      case is_list(UpdatedStorage) of
        true ->
          PID ! {reply, ok},
          loop(UpdatedStorage);
        false ->
          PID ! {reply, UpdatedStorage},
          loop(Storage)
      end;
    {PID, {get_one_value, Station ,Date, Type}} ->
      PID ! {reply, pollution:getOneValue(Station, Date, Type, Storage)},
      loop(Storage);
    {PID, {get_station_mean, Station, Type}} ->
      PID ! {reply, pollution:getStationMean(Station, Type, Storage)},
      loop(Storage);
    {PID, {get_daily_mean, Date, Type}} ->
      PID ! {reply, pollution:getDailyMean(Date, Type, Storage)},
      loop(Storage);
    {PID, {get_hourly_mean, Station, Hour, Type}} ->
      PID ! {reply, pollution:getHourlyMean(Station, Hour, Type, Storage)},
      loop(Storage);
    {PID, {get_seasonal_mean, Station, Start, End, Type}} ->
      PID ! {reply, pollution:getSeasonalMean(Station, Start, End, Type, Storage)},
      loop(Storage);
    stop -> ok
  end.

%% Client
call(Msg) ->
  pollServ ! {self(), Msg},
  receive
    {reply, Reply} -> Reply
  end.

%% Requesting operations on server via client
addStation(Name, {Long, Lat}) ->
  call({add_station, Name, {Long, Lat}}).

addValues(Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value) ->
  call({add_values, Station, {{Year, Month, Day}, {Hour, Minutes, Seconds}}, Type, Value}).

removeValue(Station, {{Year, Month, Day}, Hour}, Type) ->
  call({remove_value, Station, {{Year, Month, Day}, Hour}, Type}).

getOneValue(Station, {{Year, Month, Day}, Hour}, Type) ->
  call({get_one_value, Station, {{Year, Month, Day}, Hour}, Type}).

getStationMean(Station, Type) ->
  call({get_station_mean, Station ,Type}).

getDailyMean({Year, Month, Day}, Type) ->
  call({get_daily_mean, {Year, Month, Day}, Type}).

getHourlyMean(Station, Hour, Type) ->
  call({get_hourly_mean, Station, Hour, Type}).

getSeasonalMean(Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type) ->
  call({get_seasonal_mean, Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type}).
