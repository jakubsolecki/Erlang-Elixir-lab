%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2020 18:03
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("Jakub Solecki").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%% API
-export([]).

start_test() ->
  pollution_server:start(),
  ?assert(lists:member(pollServ, registered())).

addStation_test() ->
  ?assertEqual(ok, pollution_server:addStation("Station 1", {1,1})),
  ?assertEqual(ok, pollution_server:addStation("Station 3", {2,2})),
  ?assertMatch({error, _}, pollution_server:addStation("Station 1", {1,2})),
  ?assertMatch({error, _}, pollution_server:addStation("Station 2", {1,1})).

addValues_test() ->
  ?assertEqual(ok, pollution_server:addValues({1,1}, calendar:local_time(), "temp", 15)),
  ?assertMatch({error, _}, pollution_server:addValues({1,1}, calendar:local_time(), "temp", 15)),
  ?assertMatch({error, _}, pollution_server:addValues({"Station 2"}, calendar:local_time(), "PM 2.5", 100)),
  ?assertEqual(ok, pollution_server:addValues("Station 3", calendar:local_time(), "temp", 25)).

getOneValue_test() ->
  {{Y, M, D}, {H, _, _}} = calendar:local_time(),
  ?assertEqual(15, pollution_server:getOneValue("Station 1", {{Y, M, D}, H}, "temp")),
  ?assertEqual(15, pollution_server:getOneValue({1, 1}, {{Y, M, D}, H}, "temp")),
  ?assertMatch({error, _}, pollution_server:getOneValue("Station 1", {{Y, M, D}, H}, "PM 10")),
  ?assertMatch({error, _}, pollution_server:getOneValue("Station 2", {{Y, M, D}, H}, "temp")),
  ?assertMatch({error, _}, pollution_server:getOneValue("Station 3", {{Y, M, D}, H}, "PM 2.5")).

getStationMean_test() ->
  ?assertEqual(15.0, pollution_server:getStationMean({1, 1}, "temp")),
  ?assertEqual(15.0, pollution_server:getStationMean("Station 1", "temp")),
  ?assertMatch({error, _}, pollution_server:getStationMean({1, 1}, "PM 10")),
  ?assertMatch({error, _}, pollution_server:getStationMean({2, 2}, "tmp")),
  ?assertMatch({error, _}, pollution_server:getStationMean("Platform 9 i 3/4", "PM 10")).

getDailyMean_test() ->
  {{Y, M, D}, _} = calendar:local_time(),
  ?assertEqual(20.0, pollution_server:getDailyMean({Y, M, D}, "temp")),
  ?assertMatch({error, _}, pollution_server:getDailyMean({Y, M, D}, "PM 10")).

getHourlyMean_test() ->
  {_, {H, _, _}} = calendar:local_time(),
  ?assertEqual(25.0, pollution_server:getHourlyMean("Station 3", H, "temp")),
  ?assertEqual(25.0, pollution_server:getHourlyMean({2, 2}, H, "temp")),
  ?assertMatch({error, _}, pollution_server:getHourlyMean("Station 2", H, "temp")),
  ?assertMatch({error, _}, pollution_server:getHourlyMean("Station 1", H, "PM 2.5")).

getSeasonalMean_test() ->
  ?assertEqual(15.0, pollution_server:getSeasonalMean("Station 1", {2020, 1}, {2021, 1}, "temp")),
  ?assertEqual(25.0, pollution_server:getSeasonalMean({2, 2}, {2020, 1}, {2021, 1}, "temp")),
  ?assertMatch({error, _}, pollution_server:getSeasonalMean("Station 1", {2020, 1}, {2021, 1}, "PM 10")),
  ?assertMatch({error, _}, pollution_server:getSeasonalMean({1, 2}, {2002, 1}, {2019, 1}, "temp")).

removeValue_test() ->
  {{Y, M, D}, {H, _, _}} = calendar:local_time(),
  ?assertEqual(ok, pollution_server:removeValue("Station 1", {{Y, M, D}, H}, "temp")),
  ?assertMatch({error, _}, pollution_server:removeValue("Station 1", {{Y, M, D}, H}, "temp")),
  ?assertMatch({error, _}, pollution_server:removeValue("Platform 9 i 3/4", {{Y, M, D}, H}, "PM 2.5")).

stop_test() ->
  ?assertEqual(stop, pollution_server:stop()).
