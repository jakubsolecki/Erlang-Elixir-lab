%%%-------------------------------------------------------------------
%% @doc supervised_pollution_server public API
%% @end
%%%-------------------------------------------------------------------

-module(supervised_pollution_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    supervised_pollution_server_sup:start_link().

stop(_State) ->
    ok.
