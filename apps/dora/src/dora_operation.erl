%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 下午10:46
%%%-------------------------------------------------------------------
-module(dora_operation).
-author("x0e590af").

%% API
-export([enter_loop/3]).


%% socket state
-record(state, {
  socket :: inet:socket(),
  peername :: {inet:ip_address(), non_neg_integer()},
  transport :: module(),
  database = mnesis_mnesia_table0 :: atom(),
  trans = false :: boolean(),
  error = false :: boolean(),
  optlist = []:: [{atom(), integer(), [binary()]}]
}).


enter_loop(Socket, Peername, Transport) ->

%%  lager:info("connect socket : ~p",[Socket]),



  loop(#state{socket=Socket, peername=Peername, transport=Transport}).

loop(State = #state{socket=Socket, transport=Transport}) ->
  receive
      {tcp, Socket, Data} ->

        {Num, Opt} = dora_parser:parse_data(Data),

        lager:info("Num, Opt : ~p",[{Num, Opt}]),

        {Reply, NewState} = do_operation(Num, Opt),

        lager:info("do_operation : ~p",[{Reply, NewState}]),

        Transport:send(Socket, Data),
        Transport:setopts(Socket, [{active, once}]),
        loop(State);

    {tcp_closed, Socket} ->
      ok = Transport:close(Socket);
    _ ->
      ok = Transport:close(Socket)
  end.



do_operation(Num, Opt) ->

  case Opt of
      connect ->

          lager:info("connect : ~p",[Opt]),
          connect(Num),
          {ok, new};

      nodes ->
          lager:info("nodes : ~w ",[[node()|nodes()]]),
          {ok, nodes()};

      _ ->
          {ok, old}
  end.



connect(Num) ->

   Servers = [node()|nodes()],
   PoolArgs = [{name, {local, tcp_worker}}, {worker_module, tcp_worker},{size, Num}, {max_overflow, 0}],

   rpc:multicall(Servers, poolboy, start_link, [PoolArgs]).

