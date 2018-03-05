%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 一月 2018 下午6:41
%%%-------------------------------------------------------------------
-module(example_worker).
-author("x0e590af").


-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-import(lists, [reverse/1]).

-record(state, {conn}).

-define(NL, <<"\r\n">>).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  Hostname = proplists:get_value(hostname, Args),
  Port = proplists:get_value(port, Args),

  %{ok, Conn} = eredis:start_link(Hostname,Port),

  SocketOpts = [binary, {packet, line}, {active, false}, {recbuf, 1024}],
  Result = gen_tcp:connect(Hostname, Port, SocketOpts),
  case Result of
    {ok, Conn} ->
      {ok, #state{conn=Conn}};
    Error ->
      {stop, Error}
  end.



handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->

  %{reply, eredis:q(Conn, ["GET", "key1"]), State};

  gen_tcp:send(Conn, Sql),

  Result = get_request(Conn, [], 0),


  lager:info(" gen_tcp:loop data: ~p",[Result]),

  {reply, Result, State};


handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
  {reply, eredis:q(Conn, Stmt, Params), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->

  eredis:stop(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



get_request(Socket, BinaryList, Count) ->

  inet:setopts(Socket, [{packet, line}]),

  case gen_tcp:recv(Socket, Count, 10) of

      {ok, Binary} ->

          io:format("1received data: ~p~n", [Binary]),
          get_request(Socket, [Binary|BinaryList], Count);


      {error, timeout}->

          lists:reverse(BinaryList)


  end.





