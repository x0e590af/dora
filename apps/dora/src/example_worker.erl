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

  Result = case gen_tcp:send(Conn, Sql) of
  ok ->

    read_resp(Conn);


  Error ->
    Error
  end,

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
set_default({Prop, Value}, PropList) ->
  case proplists:is_defined(Prop, PropList) of
    true ->
      PropList;
    false ->
      [{Prop, Value} | PropList]
  end.

set_defaults(Defaults, PropList) ->
  lists:foldl(fun set_default/2, PropList, Defaults).

strip(B) when is_binary(B) ->
  S = size(B) - size(?NL),
  <<B1:S/binary, _/binary>> = B,
  B1.



read_resp(Socket) ->
  inet:setopts(Socket, [{packet, line}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, Line} ->

      lager:info(" gen_tcp:recv data: ~p",[Line]),

      case Line of
        <<"*", Rest/binary>> ->
          Count = list_to_integer(binary_to_list(strip(Rest))),
          read_multi_bulk(Socket, Count, []);
        <<"+", Rest/binary>> ->
          {ok, strip(Rest)};
        <<"-", Rest/binary>> ->
          {error, strip(Rest)};
        <<":", Size/binary>> ->
          {ok, list_to_integer(binary_to_list(strip(Size)))};
        <<"$", Size/binary>> ->
          Size1 = list_to_integer(binary_to_list(strip(Size))),
          Da = read_body(Socket, Size1),
          lager:info(" read_body:recv data: ~p",[Line]);
        <<"\r\n">> ->
          read_resp(Socket);
        Uknown ->
          {unknown, Uknown}
      end;
    Error ->
      Error
  end.

read_body(_Socket, -1) ->
  {ok, null};
read_body(_Socket, 0) ->
  {ok, <<>>};
read_body(Socket, Size) ->
  inet:setopts(Socket, [{packet, raw}]),
  %gen_tcp:recv(Socket, Size).
  case gen_tcp:recv(Socket, Size) of
  {ok, Line} ->
    Line
%%  lager:info(" read_body:recv data: ~p",[Line])
   end.

read_multi_bulk(_Data, 0, Acc) ->
  lists:reverse(Acc);
read_multi_bulk(Socket, Count, Acc) ->
  Acc1 = [read_resp(Socket) | Acc],
  read_multi_bulk(Socket, Count-1, Acc1).

build_request(Args) when is_list(Args) ->
  Count = length(Args),
  F = fun(V) -> ["$", to_part(length(to_part(V))), ?NL, to_part(V), ?NL] end,
  Args1 = lists:map(F, Args),
  ["*", to_part(Count), ?NL, Args1, ?NL].

to_part(A) when is_atom(A) ->
  string:to_upper(atom_to_list(A));
to_part(B) when is_binary(B) ->
  binary_to_list(B);
to_part(I) when is_integer(I) ->
  integer_to_list(I);
to_part(L) when is_list(L) ->
  L.