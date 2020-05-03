%%%-------------------------------------------------------------------
%%% @author jakubs
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. maj 2020 18:59
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("jakubs").

-behaviour(gen_server).

%% API
-export([start_link/0, addStation/2, addValues/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getHourlyMean/3, getSeasonalMean/4, stop/0, crash/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%% Custom types to simplify functions' signatures
-type full_date() :: {{Year :: integer(), Month :: integer(), Day :: integer()}, {Hour :: integer(),
  Minute :: integer(), Second :: integer()}}.
-type coords() :: {Lon :: float(), Lat :: float()}.
-type hour_date() :: {{Year :: integer(), Month :: integer(), Day :: integer()}, Hour :: integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
%%-spec(start_link() ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, pollution:createMonitor(), []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%%-spec(init(Args :: term()) ->
%%  {ok, State :: #pollution_gen_server_state{}} | {ok, State :: #pollution_gen_server_state{}, timeout() | hibernate} |
%%  {stop, Reason :: term()} | ignore).
init(Monitor) ->
  {ok, Monitor}.

%% SYNCHRONOUS REQUESTS
-spec(addStation(Name :: string(), Coords :: coords()) -> term()).
addStation(Name, Coords) -> gen_server:cast(?MODULE, {addStation, [Name, Coords]}).

-spec(addValues(Station :: term(), Date :: full_date(), Type :: string(), Value :: number()) -> term()).
addValues(Station, Date, Type, Value) -> gen_server:cast(?MODULE, {addValues, [Station, Date, Type, Value]}).

-spec(removeValue(Station :: coords() | string(), Date :: hour_date(), Type :: string()) -> term()).
removeValue(Station, Date, Type) -> gen_server:cast(?MODULE, {removeValue, [Station, Date, Type]}).

crash() -> gen_server:cast(?MODULE, crash).

%% ASYNCHRONOUS REQUESTS
-spec(getOneValue(Station :: coords() | string(), Date :: hour_date(), Type :: string()) -> term()).
getOneValue(Station, Date, Type) -> gen_server:call(?MODULE, {getOneValue, [Station, Date, Type]}).
getStationMean(Station, Type) -> gen_server:call(?MODULE, {getStationMean, [Station, Type]}).
getDailyMean(Date, Type) -> gen_server:call(?MODULE, {getDailyMean, {getDailyMean, [Date, Type]}}).
getHourlyMean(Station, Hour, Type) -> gen_server:call(?MODULE, {getHourlyMean, [Station, Hour, Type]}).
getSeasonalMean(Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type) ->
  gen_server:call(?MODULE, {getSeasonalMean, [Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type]}).
stop() -> gen_server:call(?MODULE, terminate).


%% @private
%% @doc Handling call messages
%%-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
%%    State :: #pollution_gen_server_state{}) ->
%%  {reply, Reply :: term(), NewState :: #pollution_gen_server_state{}} |
%%  {reply, Reply :: term(), NewState :: #pollution_gen_server_state{}, timeout() | hibernate} |
%%  {noreply, NewState :: #pollution_gen_server_state{}} |
%%  {noreply, NewState :: #pollution_gen_server_state{}, timeout() | hibernate} |
%%  {stop, Reason :: term(), Reply :: term(), NewState :: #pollution_gen_server_state{}} |
%%  {stop, Reason :: term(), NewState :: #pollution_gen_server_state{}}).
%%handle_call(_Request, _From, State = #pollution_gen_server_state{}) ->
%%  {reply, ok, State}.

handle_call({getOneValue, [Station, Date, Type]}, _From, Monitor) ->
  {reply, pollution:getOneValue(Station, Date, Type, Monitor), Monitor};
handle_call({getStationMean, [Station, Type]}, _From, Monitor) ->
  {reply, pollution:getStationMean(Station, Type, Monitor), Monitor};
handle_call({getDailyMean, {getDailyMean, [Date, Type]}}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Date, Type, Monitor), Monitor};
handle_call({getHourlyMean, [Station, Hour, Type]}, _From, Monitor) ->
  {reply, pollution:getHourlyMean(Station, Hour, Type, Monitor), Monitor};
handle_call({getSeasonalMean, [Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type]}, _From, Monitor) ->
  {reply, pollution:getSeasonalMean(Station, {StartYear, StartMonth}, {EndYear, EndMonth}, Type, Monitor), Monitor};
handle_call(terminate, _From, Monitor) ->
  {stop, normal, ok, Monitor}.


%% @private
%% @doc Handling cast messages
%%-spec(handle_cast(Request :: term(), State :: #pollution_gen_server_state{}) ->
%%  {noreply, NewState :: #pollution_gen_server_state{}} |
%%  {noreply, NewState :: #pollution_gen_server_state{}, timeout() | hibernate} |
%%  {stop, Reason :: term(), NewState :: #pollution_gen_server_state{}}).
%%handle_cast(_Request, State = #pollution_gen_server_state{}) ->
%%  {noreply, State}.
handle_cast({addStation, [Name, Coords]}, Monitor) ->
  handle_cast_result(pollution:addStation(Name, Coords, Monitor), Monitor);
handle_cast({addValues, [Station, Date, Type, Value]}, Monitor) ->
  handle_cast_result(pollution:addValues(Station, Date, Type, Value, Monitor), Monitor);
handle_cast({removeValue, [Station, Date, Type]}, Monitor) ->
  handle_cast_result(pollution:removeValue(Station, Date, Type, Monitor), Monitor);
handle_cast(crash, Monitor) -> 1 / 0, {noreply, Monitor}.

%% @private
%% @doc Handling result from functions
handle_cast_result({error, Msg}, OldMonitor) -> {noreply, Msg, OldMonitor};
handle_cast_result(UpdatedMonitor, _OldMonitor) -> {noreply, UpdatedMonitor}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, Monitor) ->
  {noreply, Monitor}.

%% @private
%%%% @doc This function is called by a gen_server when it is about to
%%%% terminate. It should be the opposite of Module:init/1 and do any
%%%% necessary cleaning up. When it returns, the gen_server terminates
%%%% with Reason. The return value is ignored.
terminate(_Reason, Monitor) ->
  erlang:display(Monitor),
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
