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
init({Ref, Socket, Transport, _Opts = []}) ->


  ok = Transport:setopts(Socket, [{active, once}]),
  ok = ranch:accept_ack(Ref),
  dora_operation:enter_loop(Socket, inet:peername(Socket), Transport).



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
