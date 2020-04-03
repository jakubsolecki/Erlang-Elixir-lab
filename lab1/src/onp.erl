%%%-------------------------------------------------------------------
%%% @author Jakub
%%% @copyright (C) 2020, <Me>
%%% @doc
%%%
%%% @end
%%% Created : 12. mar 2020 21:37
%%%-------------------------------------------------------------------
-module(onp).
-author("Jakub").

%% API
-export([onp/1]).


onp(S) -> calculateRPN(string:tokens(S, " "), []).

calculateRPN([], [Stack]) -> Stack;
calculateRPN([H | T], Stack) ->
  case re:run(H, "^[0-9]*$") /= nomatch of
    true -> calculateRPN(T, [list_to_integer(H) | Stack]);
    false -> case re:run(H, "^[0-9]+[.][0-9]+$") /= nomatch of
               true -> calculateRPN(T, [list_to_float(H) | Stack]);
               false -> calculateRPN2([H | T], Stack)
             end
  end.

calculateRPN2(["+" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [S1 + S2 | Stack]);
calculateRPN2(["-" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [S2 - S1 | Stack]);
calculateRPN2(["*" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [S1 * S2 | Stack]);
calculateRPN2(["/" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [S2 / S1 | Stack]);
calculateRPN2(["pow" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [math:pow(S2, S1) | Stack]);
calculateRPN2(["sqrt" | T], [S | Stack]) ->
  calculateRPN(T, [math:sqrt(S) | Stack]);
calculateRPN2(["sin" | T], [S | Stack]) ->
  calculateRPN(T, [math:sin(math:pi() * S / 180) | Stack]);
calculateRPN2(["cos" | T], [S | Stack]) ->
  calculateRPN(T, [math:cos(math:pi() * S / 180) | Stack]);
calculateRPN2(["tan" | T], [S | Stack]) ->
  calculateRPN(T, [math:tan(math:pi() * S / 180) | Stack]);
calculateRPN2(["double" | T], [S | Stack]) ->
  calculateRPN(T, [S * 2 | Stack]);
calculateRPN2(["avg" | T], [S1, S2 | Stack]) ->
  calculateRPN(T, [(S1 + S2) / 2 | Stack]).

