%%%-------------------------------------------------------------------
%%% @author Jakub
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mar 2020 12:26
%%%-------------------------------------------------------------------
-module(myLists).
-author("Jakub").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _) ->
  false;
contains([H | _], H) ->
  true;
contains([_ | T], X) ->
  contains(T, X).


duplicateElements([]) ->
  [];
duplicateElements([H | []]) ->
  [H, H];
duplicateElements([H | T]) ->
  [H, H | duplicateElements(T)].


sumFloats([]) ->
  0.0;
sumFloats([A]) when is_float(A) ->
  A;
sumFloats([H | T]) when is_float(H) ->
  H + sumFloats(T).
