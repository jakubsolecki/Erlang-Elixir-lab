%%%-------------------------------------------------------------------
%% @doc supervised_pollution_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(supervised_pollution_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
      #{id => process_name,
        start => {pollution_gen_server, start_link, []},
        restart => permanent,
        shutdown => 3000,
        type => worker,
        modules => [pollution_gen_server]
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
