%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2019 09:12
%%%-------------------------------------------------------------------
-module(simpletest).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

simple1_test() ->
  ?assert(true).

simple2_test() ->
  ?assertEqual(1, 1).

simple3_test_() ->
  [ fun () -> ?assertEqual(1, 1) end,
    fun () -> ?assertEqual(3, 3) end].

simple4_test_() ->
  [ ?_assertEqual(1, 1) ,
    ?_assertEqual(3, 3) ].


factorial (N) when N =< 0 -> 1;
factorial (N) -> factorial (N-1) * N.

simple5_test_() ->
  [ ?_assertEqual(factorial(1), 1) ,
    ?_assertEqual(factorial(2), 2) ,
    ?_assertEqual(factorial(3), 6) ,
%    ?_assertEqual(factorial(4), 20) ,
    ?_assertEqual(factorial(5), 120)
  ].



simple6_test_() ->
  [ ?_assertEqual(factorial(N), factorial(N-1) * N) || N <- lists:seq(1,100)  ].

simple7_test_() ->
  {inparallel,
  [ ?_assertEqual(factorial(N), factorial(N-1) * N) || N <- lists:seq(1,100)  ]
  }.



fix_test_() ->
  {setup,
    fun() -> factorial(100) end,
    fun(Fact100) ->
      [ ?_assert(Fact100 > 100),
        ?_assert(Fact100 > 100000),
        ?_assert(Fact100 > factorial(99)),
        ?_assert(Fact100 < factorial(101))
      ]
    end
  }.




