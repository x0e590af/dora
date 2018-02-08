%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 一月 2018 下午5:03
%%%-------------------------------------------------------------------
-module(dora_parser).
-author("x0e590af").

-export([
  parse_data/1,
  lower_by_list/1,
  parse_param/1
]).


-spec lower_by_list(Data::list()) ->
  {Cmd::string()}.

lower_by_list(Data) when is_list(Data) ->

  string:to_lower(Data).

parse_param(Data)  ->

  {Num, Opt} = string:to_integer(Data),

  New_opt = list_to_atom(string:strip(Opt)),

  {Num, New_opt}.



-spec parse_data(Data::binary()) ->
  {Cmd::string()}.

parse_data(Data) when is_binary(Data) ->
  parse_param(lower_by_list(
    binary_to_list(binary:part(Data, {0, byte_size(Data)-1}))
  )).




