%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2019 15:30
%%%-------------------------------------------------------------------
-module(pollution_server_fix_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

fix_test_() ->
  {setup,
    fun() -> pollution_server:start_link() end,
    fun(_) -> pollution_server:stop() end,
    fun(_) ->
    [ ?_assertEqual(ok,pollution_server:addStation("stacja", {1,1})),
      ?_assertEqual(ok,pollution_server:addStation("stacja2", {1,2})),
%      ?_assertEqual(ok,pollution_server:addStation("stacja", {1,2})),
      ?_assertMatch({error, _},pollution_server:addStation("stacja", {1,1}))
     ]
    end
  }.




