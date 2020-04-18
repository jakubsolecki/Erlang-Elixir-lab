%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2019 15:30
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


startServer_test() ->
  pollution_server:start_link(),
  ?assert(lists:member(pollution_server, registered())).

addStation_test() ->
  ?assertEqual(ok,pollution_server:addStation("stacja", {1,1})).

addStation2_test() ->
  ?assertEqual(ok,pollution_server:addStation("stacja2", {1,2})),
  ?assertMatch({error, _},pollution_server:addStation("stacja", {1,1})).

stopServer_test() ->
  pollution_server:stop(),
  timer:sleep(50),
  ?assert(not lists:member(pollution_server, registered())).

