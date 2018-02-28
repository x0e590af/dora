%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2018 下午6:16
%%%-------------------------------------------------------------------
-module(dora_protocol).
-author("x0e590af").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



-record(state, {socket, transport}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
%init([]) -> {ok, undefined}.

init({Ref, Socket, Transport, _Opts = []}) ->

  lager:info("connect ~w",[Socket]),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}]),
  gen_server:enter_loop(?MODULE, [],
    #state{socket=Socket, transport=Transport}).

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) when byte_size(Data) > 1 ->



    lager:info("ranch handle dataxxx : ~p",[Data]),

    %% 获取数据库的配置
    {ok, Pools} = application:get_env(dora, pools),
    Nodes = hash_ring:list_to_nodes( [Name||{Name, SizeArgs, WorkerArgs}<-Pools]),

    Ring0 = hash_ring:make(Nodes),

    {ok ,  {hash_ring_node,HashRingVal,HashRingKey,1} }= hash_ring:find_node(item_1, Ring0),



    lager:info("hash ring : ~p",[HashRingVal]),

    Result = poolboy:transaction(HashRingVal, fun(Worker) ->
          gen_server:call(Worker, {squery, Data})
     end),

    lager:info("ranch handle Result : ~p",[Result]),

    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, reverse_binary(Data)),
  {noreply, State};


handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
  {stop, Reason, State};
handle_info(timeout, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {stop, normal, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.

reverse_binary(B) when is_binary(B) ->
  [list_to_binary(binary_to_list(
    binary:part(B, {0, byte_size(B)-2})
  )), "\r\n"].