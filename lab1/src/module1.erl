%%%-------------------------------------------------------------------
%%% @author Jakub
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mar 2020 12:03
%%%-------------------------------------------------------------------
-module(module1).
-author("Jakub").

%% API
-export([power/2]).

power(_, 0) -> 1;
power(A, B) when B > 0 ->
  A * power(A, B - 1).

