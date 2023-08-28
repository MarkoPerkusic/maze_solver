-module(scout).
-behaviour(gen_server).


-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ---------------------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------------------

start_link(ParentPid) ->
  Maze_map = load_maze(),
  gen_server:start_link(
      {local, ?MODULE},
      ?MODULE,
      [{0, 0, Maze_map, ParentPid, []}],
      []
  ).

stop(Pid) ->
  gen_server:cast(Pid, stop).


%% ---------------------------------------------------------------------------
%% gen_server functions
%% ---------------------------------------------------------------------------

init([{X, Y, Map, ParentPid, Path}] = State) ->
  Free_cells = check_next_move(X + 1, Y + 1, Map),
  NewState = case length(Free_cells) of
    0 ->
      io:format("Dead end reached"),
      ParentPid ! {self(), dead_end},
      State;
    1 ->
      {NewX, NewY, _} = Free_cells,
      {NewX, NewY, Map, ParentPid, [{NewX, NewY} | Path]};
    _ ->
      spawn_scouts(Free_cells, Map, ParentPid, []),
      State
  end,
  {ok, NewState}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

handle_cast(explore, State) ->
  {noreply, State};
handle_cast(finish, State) ->
  %% The finish has been reached
  {noreply, State};
handle_cast(dead_end, State) ->
  %% The spawned process has reached dead end
  {noreply, State};
handle_cast(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.

terminate(_Info, _State) ->
  error_logger:info_msg("TERMINATING!~n"),
  ok.

code_change(_Old, State, _Additional) ->
  {ok, State}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

load_maze() ->
  %% To be implemented, for now return the hardcoded one
  [
    ["#######"],
    ["#*#***#"],
    ["**#*#*#"],
    ["#*#*#*#"],
    ["#***#**"],
    ["#*###*#"],
    ["#######"]
  ].

check_next_move(1, Y, Map) ->
  %% The first line is upper wall of maze,
  %% therefore it can be skipped
  check_next_move(2, Y, Map);
check_next_move(X, _, Map) when X == length(Map) ->
  %% The last line is lower wall,
  %% normally our position should never be equal to that one
  throw("Crashed in to the wall!!!");
check_next_move(X, Y, Map) ->
  Forward_cell = {lists:nth(Y, lists:nth(X, Map)), X, Y},
  Up_cell = {lists:nth(Y, lists:nth(X + 1, Map)), X + 1, Y},
  Down_cell = {lists:nth(Y, lists:nth(X - 1, Map)), X - 1, Y},
  Check_for_finish = fun(Val, PosX, PosY, Maze) ->
    case PosY == length(lists:nth(1, Maze)) andalso Val == "*" of
    true ->
      finish;
    false ->
      {Val, PosX, PosY}
    end
  end,
  [Check_for_finish(Value, XPos, YPos, Map)
    || _ = {Value, XPos, YPos} <- [Forward_cell, Up_cell, Down_cell], Value == "*"].


spawn_scouts([], _Map, _ParentPid, _Path) ->
  ok;
spawn_scouts({_, X, Y} = _Free_cells, Map, ParentPid, Path) ->
  spawn_link(?MODULE, init, [X, Y, Map, ParentPid, Path]).