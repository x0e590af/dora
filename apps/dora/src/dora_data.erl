%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 一月 2018 下午5:59
%%%-------------------------------------------------------------------

-module(dora_data).
-author("x0e590af").


-include("config.hrl").

%% API
-export([create/0,insert/2,lookall/0,delete/1]).

%%%===================================================================
%%% API
%%%===================================================================


create()->

  %% 创建表
  case  lists:member(user_info,  mnesia:system_info(tables)) of
    true ->
      already_exists;
    _ ->
      mnesia:create_table(user_info,[{attributes, record_info(fields,user_info)}])
  end.

insert(Pid,Socket)->
  mnesia:dirty_write(#user_info{pid=Pid,socket=Socket}).


lookall()->
  mnesia:dirty_select(user_info,[{#user_info{pid='$1',socket = '$2'},[],['$1']}]).


delete(Pid)->
  mnesia:dirty_delete(user_info,Pid).

%%
%%delete_schema() ->
%%  mnesia:stop(),
%%  mnesia:delete_schema([node()]),
%%  mnesia:start().

