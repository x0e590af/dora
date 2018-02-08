%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 一月 2018 上午11:16
%%%-------------------------------------------------------------------
-module( if).
-author("x0e590af").



sync_pipeline([FirstFunc|TailFuncList]) ->
  sync_pipeline(TailFuncList, FirstFunc()).

sync_pipeline([HFunc|TailFuncList], LastResult) ->
  case HFunc(LastResult) of
    %% 处理正确结果
    NewResult when NewResult =:= ok; NewResult =:= true -> sync_pipeline(TailFuncList, NewResult);
    {Rs, _Data} = NewResult when Rs =:= ok; Rs =:= true -> sync_pipeline(TailFuncList, NewResult);

    %% 处理错误结果
    NewResult when NewResult =:= error; NewResult =:= err; NewResult =:= false -> NewResult;
    {Rs, _Data} = NewResult when Rs =:= error; Rs =:= err; Rs =:= false -> NewResult
  end;
sync_pipeline([], LastResult) ->
  LastResult.




register(UserInput) ->
  sync_pipeline([
    fun() -> validator:validate(UserInput) end,
    fun(_Result) -> chaptcha:check(UserInput) end,
    fun(_Result) -> user_storage:is_username_exsted(UserInput#user.username) end,
  ...
  ])