%%%-------------------------------------------------------------------
%% @doc boots top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(boots_sup).

-behaviour(supervisor).


-export([start/0, stop/0, squery/2, equery/3]).
-export([stop/1]).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

stop(_State) ->
    ok.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Pools} = application:get_env(boots, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
        {worker_module, example_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
                                  end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
                                  end).

%%====================================================================
%% Internal functions
%%====================================================================
