%%%-------------------------------------------------------------------
%%% @author Jakub Solecki
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. kwi 2020 20:39
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Jakub Solecki").

%% API
-export([start/0, stop/0, play/1]).

pong() ->
  receive
    {Pid, A} ->
      case A > 0 of
        true ->
          io:format("Got it! ~w left~n",[A-1]),
          timer:sleep(1000),
          Pid ! {self(), A-1},
          pong();
        false -> io:format("Finished!~n"), pong()
      end;
    seppuku -> io:format("Time to say goodbye!~n"), ok
  after 20000 -> io:format("20 seconds have passed. Time to say goodbye!~n"), ok
  end.

ping(N) ->
  receive
    {Pid, A} ->
      case A > 0 of
        true ->
          io:format("Got it! ~w left. Total: ~w~n",[A-1, N+A]),
          timer:sleep(1000),
          Pid ! {self(), A-1},
          ping(N + A);
        false -> io:format("Finished!~n"), pong()
      end;
    seppuku -> io:format("Time to say goodbye!~n"), ok
  after 20000 -> io:format("20 seconds have passed. Time to say goodbye!~n"), ok
  end.

start() ->
  register(ping, spawn(fun() -> ping(0) end)),
  register(pong, spawn(fun() -> pong() end)).

stop() ->
  ping ! seppuku,
  pong ! seppuku.

play(N) -> ping ! {pong, N}.
