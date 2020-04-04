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

loop() ->
  receive
    {init, A, P} ->
      io:format("Let's do this!~n"),
       whereis(P) ! {self(), A},
        loop();
    {Pid, A} ->
      case {is_pid(Pid), A > 0} of
        {true, true} ->
          io:format("Got it! ~w left~n",[A-1]),
          timer:sleep(1000),
          Pid ! {self(), A-1},
          loop();
        {true, false} -> io:format("Finished!~n"), loop()
      end;
    _ -> loop();
    seppuku -> io:format("Time to say goodbye!~n"), ok
  after 20000 -> io:format("20 seconds have passed. Time to say goodbye!~n"), ok
  end.

loop(N) ->
  receive
    {init, A, P} ->
      io:format("Let's do this!~n"),
      whereis(P) ! {self(), A},
      loop(N);
    {Pid, A} ->
      case {is_pid(Pid), A > 0} of
        {true, true} ->
          io:format("Got it! ~w left~n Total: ~w~n",[A-1, N+A]),
          timer:sleep(1000),
          Pid ! {self(), A-1},
          loop(N + A);
        {true, false} -> io:format("Finished!~n"), loop()
      end;
    _ -> loop(N);
    seppuku -> io:format("Time to say goodbye!~n"), ok
  after 20000 -> io:format("20 seconds have passed. Time to say goodbye!~n"), ok
  end.

start() ->
  register(ping, spawn(fun() -> loop(0) end)),
  register(pong, spawn(fun() -> loop() end)).

stop() ->
  whereis(ping) ! seppuku,
  whereis(pong) ! seppuku.

play(N) -> whereis(ping) ! {init, N, pong}.
