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

%% behaviour
-behaviour(ranch_protocol).
-behaviour(gen_fsm).


%% API
-export([start_link/4]).
-export([init/1]).

-export([event/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-include("config.hrl").


start_link(Ref,Socket,Transport,Opts) ->

  %% start the process syncronously to avoid deadlock
  proc_lib:start_link(?MODULE, init, [[Ref, Socket, Transport, Opts]]).

init([Ref, Socket, Transport, _Opts=[]]) ->

  ok = proc_lib:init_ack({ok,self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket,[{active,once}]),
  gen_fsm:enter_loop(?MODULE,[], event,
    #state{ref=Ref, socket=Socket, transport=Transport}).

event({tcp,_Port,Data}, #state{transport=Transport, socket=Socket}=State) ->
  %% echo the data back to the user

  lager:info("client data : ~p", [Data]),

  Transport:send(Socket,Data),
  {next_state, event, State}.

handle_event(Event, StateName, State) ->

  {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->

  {reply, ok, StateName, State}.

handle_info({tcp_closed,_Port}, _StateName, #state{transport=Transport, socket=Socket}=State) ->
  %% the connection was closed
  Transport:close(Socket),
  {stop, _StateName, State};


handle_info(Info={tcp,_Port,_Data},StateName,#state{socket=Socket, transport=Transport}=State) ->
  %% got some data from the network, pass it to our gen_fsm event
  gen_fsm:send_event(self(),Info),
  Transport:setopts(Socket, [{active, once}]),

  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_Oldsvn, StateName, State, _Extra) ->
  {ok, StateName, State}.
