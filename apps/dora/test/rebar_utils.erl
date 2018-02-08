%%%-------------------------------------------------------------------
%%% @author x0e590af
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 一月 2018 下午7:50
%%%-------------------------------------------------------------------
-module(rebar_utils).
-author("x0e590af").

-export([sh/2]).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->

  DefaultOptions = [use_stdout, abort_on_error],
  Options = [expand_sh_flag(V)
    || V <- proplists:compact(Options0 ++ DefaultOptions)],

  ErrorHandler = proplists:get_value(error_handler, Options),
  OutputHandler = proplists:get_value(output_handler, Options),

  Command = patch_on_windows(Command0, proplists:get_value(env, Options, [])),
  PortSettings = proplists:get_all_values(port_settings, Options) ++
    [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
  Port = open_port({spawn, Command}, PortSettings),

  case sh_loop(Port, OutputHandler, []) of
    {ok, _Output} = Ok ->
      Ok;
    {error, {_Rc, _Output}=Err} ->
      ErrorHandler(Command, Err)
  end.

%% We use a bash shell to execute on windows if available. Otherwise we do the
%% shell variable substitution ourselves and hope that the command doesn't use
%% any shell magic. Also the port doesn't seem to close from time to time
%% (mingw).
patch_on_windows(Cmd, Env) ->
  case os:type() of
    {win32,nt} ->
      case find_executable("bash") of
        false -> Cmd;
        Bash ->
          Bash ++ " -c \"" ++ Cmd ++ "; echo _port_cmd_status_ $?\" "
      end;
    _ ->
      lists:foldl(fun({Key, Value}, Acc) ->
        expand_env_variable(Acc, Key, Value)
                  end, Cmd, Env)
  end.

find_executable(Name) ->
  case os:find_executable(Name) of
    false -> false;
    Path ->
      "\"" ++ filename:nativename(Path) ++ "\""
  end.

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
  ReOpts = [global, {return, list}],
  VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", ReOpts),
  R1 = re:replace(InStr, "\\\$" ++ VarName ++ "\\s", VarValue ++ " ",
    [global]),
  R2 = re:replace(R1, "\\\$" ++ VarName ++ "\$", VarValue),
  re:replace(R2, "\\\${" ++ VarName ++ "}", VarValue, ReOpts).


%% ====================================================================
%% Internal functions
%% ====================================================================
log_msg_and_abort(_Message) ->
  fun(_Command, {_Rc, _Output}) ->
    halt(1)
  end.

log_and_abort(_Command, {_Rc, _Output}) ->
  halt(1).


expand_sh_flag(return_on_error) ->
  {error_handler,
    fun(_Command, Err) ->
      {error, Err}
    end};
expand_sh_flag({abort_on_error, Message}) ->
  {error_handler,
    log_msg_and_abort(Message)};
expand_sh_flag(abort_on_error) ->
  {error_handler,
    fun log_and_abort/2};
expand_sh_flag(use_stdout) ->
  {output_handler,
    fun(Line, Acc) ->
      [Line | Acc]
    end};
expand_sh_flag({use_stdout, false}) ->
  {output_handler,
    fun(Line, Acc) ->
      [Line | Acc]
    end};
expand_sh_flag({cd, _CdArg} = Cd) ->
  {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
  {port_settings, Env}.

sh_loop(Port, Fun, Acc) ->
  receive
    {Port, {data, {_, "_port_cmd_status_ " ++ Status}}} ->
      (catch erlang:port_close(Port)), % sigh () for indentation
      case list_to_integer(Status) of
        0  -> {ok, lists:flatten(Acc)};
        Rc -> {error, Rc}
      end;
    {Port, {data, {eol, Line}}} ->
      sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
    {Port, {data, {noeol, Line}}} ->
      sh_loop(Port, Fun, Fun(Line, Acc));
    {Port, {exit_status, 0}} ->
      {ok, lists:flatten(lists:reverse(Acc))};
    {Port, {exit_status, Rc}} ->
      {error, {Rc, lists:flatten(lists:reverse(Acc))}}
  end.


%%$ cat test.sh
%%#!/bin/bash
%%echo "arg $1"
%%echo "env ${X}"
%%
%%$ erlc rebar_utils.erl
%%$ erl
%%Erlang R14B04 (erts-5.8.5)  [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
%%
%%Eshell V5.8.5  (abort with ^G)
%%1> rebar_utils:sh("./test.sh yufeng",[{env,[{"X", "yufeng"}]}]).
%%{ok,"arg yufeng\nenv yufeng\n"}
%%2> os:putenv("X", "yufeng").
%%true
%%3> os:cmd("./test.sh yufeng").
%%"arg yufeng\nenv yufeng\n"
