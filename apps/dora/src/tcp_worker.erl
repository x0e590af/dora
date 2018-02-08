%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 一月 2018 下午6:41
%%%-------------------------------------------------------------------
-module(tcp_worker).
-author("x0e590af").

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {conn}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->

  process_flag(trap_exit, true),

  {ok, Socket} = gen_tcp:connect("localhost", 9527, [binary, {packet, 0}]),

  ok = gen_tcp:send(Socket, "rpush list-key item"),

  %% lager:info("Socket : ~p", [Socket]),

  ok = gen_tcp:close(Socket),

  {ok, #state{conn=Socket}}.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->

  ok = gen_tcp:send(Conn, "xxxxxxx"),
  receive
    {tcp, Socket, Bin} ->
      io:format("Client received binary = ~p~n", [Bin])

  end,

  {reply, ok , State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
  {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->

  ok = gen_tcp:close(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



wait() ->
  receive
    die -> void
  end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].