%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2018 下午11:17
%%%-------------------------------------------------------------------
-module(processes).
-author("x0e590af").

-export([max/1]).

max(N) ->
  Max = erlang:system_info(process_limit),
  io:format("Maxmium allowed process is ~p ~n", [Max]),
  statistics(runtime),
  statistics(wall_clock),
  L = for(1, N, fun() ->
    spawn(fun() ->
      io:format("ssss ~p",[N]),
      wait()
          end) end),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  lists:foreach(fun(Pid) -> Pid ! die end, L),
  U1 = Time1 * 1000 / N,
  U2 = Time2 * 1000 /N,
  io:format("Process spawn time=~p (~p) microseconds ~n", [U1, U2]).

wait() ->
  receive
    die -> void
  end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].