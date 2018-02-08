%% TCP echo server demo
-module(echo_server).
-export([start/0]).

start() ->
    Pid = spawn(fun() ->
        {ok, ListenSocket} = gen_tcp:listen(2345, [binary, {active, false}, {reuseaddr, true}, {backlog, 64}]),
        spawn(fun() ->
            accept(ListenSocket)
              end),
        timer:sleep(infinity)
                end),
    {ok, Pid}.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() ->
        accept(ListenSocket)
          end),
    trace("connect ~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    gen_tcp:send(Socket, "> "),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"bye", _/binary>>} ->
            gen_tcp:send(Socket, "bye bye!\n"),
            gen_tcp:close(Socket);
        {tcp, Socket, Message} ->
            gen_tcp:send(Socket, Message),
            loop(Socket);
        {tcp_closed, Socket} ->
            trace("close ~p~n", [Socket]),
            gen_tcp:close(Socket);
        {tcp_error, Socket, Reason} ->
            trace("Handle error ~p on ~p~n", [Reason, Socket])
    end.

trace(Message, Args) ->
    io:format(Message, Args).