%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2020 16:28
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Jakub Solecki").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


%% API
-export([]).


createMonitor_test() ->
  [Stations, Readings] = pollution:createMonitor(),

  ?assert(is_map(Stations)),
  ?assert(dict:is_empty(Readings)).


addStation_test() ->
  P1 = pollution:createMonitor(),
  [Stations, Readings] = pollution:addStation("Station 1", {1, 1}, P1),

  ?assert(maps:is_key("Station 1", Stations)),
  ?assert(maps:is_key({1 ,1}, Stations)),
  ?assertMatch({error, _}, pollution:addStation("Station 2", {1, 1}, [Stations, Readings])).


addValues_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),
  P3 = pollution:addValues({1, 1}, Date, "temp", 10, P2),
  [Stations, Readings] = P3,

  ?assert(true, dict:is_key({"Station 1", {1, 1}}, Readings)),
  ?assertMatch({error, _}, pollution:addValues("Station 1", Date, "temp", 20, P3)).


removeValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),

  ?assertMatch({error, _}, pollution:removeValue({1, 1}, Date, "temp", P2)),

  P3 = pollution:addValues({1, 1}, Date, "temp", 10, P2),
  {{Y, M, D}, {H ,_, _}} = Date,

  ?assertMatch({error, _}, pollution:removeValue({2, 2}, {{Y, M, D}, H}, "temp", P3)),

  P4 = pollution:removeValue("Station 1", {{Y, M, D}, H}, "temp", P3),
  [Stations, Readings] = P4,

  ?assertMatch([], dict:fetch({"Station 1", {1, 1}}, Readings)),
  ?assertMatch({error, _}, pollution:removeValue("Station 1", {{Y, M, D}, H}, "temp", P4)).

getOneValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),
  {{Y, M, D}, {H, _, _}} = Date,

  ?assertMatch({error, _}, pollution:getOneValue("Station 3", {{Y, M, D}, H}, "temp", P2)),
  ?assertMatch({error, _}, pollution:getOneValue("Station 1", {{Y, M, D}, H}, "temp", P2)),

  P3 = pollution:addValues({1, 1}, Date, "temp", 10, P2),

  ?assertMatch({error, _}, pollution:getOneValue("Station 1", {{Y, M, D}, H}, "PM 10", P3)),
  ?assertEqual(10, pollution:getOneValue("Station 1", {{Y, M, D}, H}, "temp", P3)).


getStationMean_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),

  ?assertMatch({error, _}, pollution:getStationMean("Station 1", "temp", P2)),

  Date = calendar:local_time(),
  {{Y, Mo, D}, {H , Mi, S}} = Date,
  P3 = pollution:addValues({1, 1}, Date, "temp", 10, P2),
  H2 = H + 1,
  P4 = pollution:addValues({1, 1}, {{Y, Mo, D}, {H2 , Mi, S}}, "temp", 15, P3),

  ?assertEqual(12.5, pollution:getStationMean("Station 1", "temp", P4)),
  ?assertMatch({error, _}, pollution:getStationMean({8, 8}, "temp", P4)).


getDailyMean_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),
  {{Y, Mo, D}, {H, Mi, S}} = Date,
  P3 = pollution:addValues({1, 1}, Date, "temp", 3, P2),
  H2 = H + 1,
  P4 = pollution:addValues({1, 1}, {{Y, Mo, D}, {H2 , Mi, S}}, "temp", 15, P3),
  H3 = H2 + 1,
  P5 = pollution:addStation("Station 2", {2, 2}, P4),
  P6 = pollution:addValues({1, 1}, {{Y, Mo, D}, {H3 , Mi, S}}, "temp", 15, P5),

  ?assertEqual(11.0, pollution:getDailyMean({Y, Mo, D}, "temp", P6)),
  ?assertMatch({error, _}, pollution:getDailyMean({Y, Mo, D}, "PM 10", P6)),
  ?assertMatch({error, _}, pollution:getDailyMean({Y + 1, Mo + 1, D + 1}, "temp", P6)).


getHourlyMean_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),
  {{Y, Mo, D}, {H, Mi, S}} = Date,
  P3 = pollution:addValues({1, 1}, Date, "temp", 3, P2),
  D2 = D + 1,
  P4 = pollution:addValues({1, 1}, {{Y, Mo, D2}, {H , Mi, S}}, "temp", 15, P3),
  D3 = D2 + 1,
  P5 = pollution:addValues({1, 1}, {{Y, Mo, D3}, {H , Mi, S}}, "temp", 15, P4),

  ?assertEqual(11.0, pollution:getHourlyMean("Station 1", H, "temp", P5)),
  ?assertMatch({error, _}, pollution:getHourlyMean({0, 0}, H, "temp", P5)),
  ?assertMatch({error, _}, pollution:getHourlyMean({1, 1}, H, "PM 2.5", P5)).


getSeasonalMean_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation("Station 1", {1, 1}, P1),
  Date = calendar:local_time(),
  {{Y, Mo, D}, {H, Mi, S}} = Date,
  P3 = pollution:addValues({1, 1}, Date, "temp", 3, P2),
  D2 = D + 1,
  P4 = pollution:addValues({1, 1}, {{Y, Mo, D2}, {H , Mi, S}}, "temp", 15, P3),
  D3 = D2 + 1,
  P5 = pollution:addValues({1, 1}, {{Y, Mo, D3}, {H , Mi, S}}, "temp", 15, P4),

  ?assertEqual(11.0, pollution:getSeasonalMean("Station 1", {2020, 1}, {2020, 12}, "temp", P5)),
  ?assertMatch({error, _}, pollution:getSeasonalMean("Station 2", {2020, 1}, {2020, 12}, "temp", P5)),
  ?assertMatch({error, _}, pollution:getSeasonalMean("Station 1", {2020, 1}, {2020, 12}, "PM 10", P5)).
