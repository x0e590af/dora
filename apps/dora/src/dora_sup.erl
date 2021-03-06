%%%-------------------------------------------------------------------
%% @doc dora top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dora_sup).

-behaviour(supervisor).

%% API
-export([start/0, stop/0, squery/2, equery/3]).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% 宏变量定义
-define(SERVER, ?MODULE).
-define(DEF_PORT, 5555).


%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

start_link() ->

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================


%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->



    %% 获取端口配置参数，找不到时返回默认端口 ?DEF_PORT

    {ok, ListenPort} = application:get_env(dora,listen_port),

    lager:info("listen_port : ~p", [list_to_integer(ListenPort)]),

    ListenerSpec = ranch:child_spec(
        dora,
        100,
        ranch_tcp,
        [{port, list_to_integer(ListenPort)}, {max_connections, 10000}],
        dora_protocol,
        []
    ),


    %% 获取数据库的配置
    {ok, Pools} = application:get_env(dora, pools),

    lager:info("Pools : ~p", [Pools]),

    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
            {worker_module, example_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                          end, Pools),

    Procs = [ListenerSpec|PoolSpecs],
    {ok, {{one_for_one, 10, 10}, Procs}}.



%%====================================================================
%% Internal functions
%%====================================================================

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
                                  end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
                                  end).
