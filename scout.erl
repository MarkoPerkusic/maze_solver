-module(scout).
-behaviour(gen_server).
-record(state, { x_y_cord, map, parent, path}).


-export([start_link/1, stop/1, show_progress/0, move/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1, code_change/3]).


%% ---------------------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------------------

start_link(ParentPid) ->
  Maze_map = load_maze(),
  io:format("ParentPid ~p~n", [ParentPid]),
  gen_server:start_link(
      {local, ?MODULE},
      ?MODULE,
      [{0, 0, Maze_map, ParentPid, []}],
      []
  ).

stop(Pid) ->
  gen_server:cast(Pid, stop).

show_progress() ->
  gen_server:call(?MODULE, status).

move() ->
  gen_server:cast(?MODULE, explore).


%% ---------------------------------------------------------------------------
%% gen_server functions
%% ---------------------------------------------------------------------------

init([{PosX, PosY, MazeMap, ParentPid, Path}]) ->
  io:format("-------------------------------My PID is: ~p---------------------------------~n", [self()]),
  io:format("State is ~p~n~p~n~p~n~p~n~p~n", [PosX, PosY, MazeMap, ParentPid, Path]),
  gen_server:cast(self(), explore),
  {ok, #state{
    x_y_cord = {PosX, PosY},
    map = MazeMap,
    parent = ParentPid,
    path = Path}
  }.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(status, _From, State) ->
  io:format("Current status is: ~p~n!", [State#state.path]),
  {reply, State#state.path, State};
handle_call(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

handle_cast(explore,
    #state{
      x_y_cord = {X, Y},
      map = Map,
      parent = _ParentPid,
      path = Path
    } = State) ->
  io:format("State~p~n", [State]),
  io:format("~p~nExploring!", [self()]),
  Ra = check_next_move(X, Y, Map),
  io:format("Ra~p~n", [Ra]),
  Result = lists:filter(fun({_, Xpos, Ypos}) -> not lists:member({Xpos, Ypos}, Path) end, Ra),
  io:format("Result: ~p~n", [Result]),
  {Verdict, NewState} = case Result of
    [] ->
      io:format("------------------------> dead~n"),
      {dead_end, State};
    [{finish, NewX, NewY}] ->
      io:format("------------------------> finish~n"),
      NewS = State#state{x_y_cord = {NewX, NewY}, path = [{NewX, NewY} | Path]},
      {finish, NewS};
    [{"*", NewX, NewY}] ->
      io:format("------------------------> explore~n"),
      NewS = State#state{x_y_cord = {NewX, NewY}, path = [{NewX, NewY} | Path]},
      {explore, NewS};
    _ ->
      io:format("------------------------> idle~n"),
      [spawn_scouts(R, Map, self(), Path) || {_, _NewX, _NewY}  = R <- Result],
      {idle, State}
  end,
  gen_server:cast(self(), Verdict),
  {noreply, NewState};
handle_cast(idle, State) ->
  %% Process has spawned other processes to search path
  %% Waiting for the reply from spawned processes
  io:format("Going to idle state!~n"),
  {noreply, State};
handle_cast(finish, State) ->
  %% The finish has been reached
  io:format("Exit reached!"),
  io:format("Path: ~p~n", [State#state.path]),
  {noreply, State};
handle_cast(dead_end, State) ->
  %% The spawned process has reached dead end
  io:format("Dead end reached!"),
  {noreply, State};
handle_cast(stop, State) ->
  %% Terminate
  io:format("Calling terminate!"),
  terminate(State);
handle_cast(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  error_logger:info_msg("Info: ~p~n", [Info]),
  {noreply, State}.

terminate(State) ->
  io:format("TERMINATING!~n"),
  {stop, normal, State}.

code_change(_Old, State, _Additional) ->
  {ok, State}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

load_maze() ->
  %% To be implemented, for now return the hardcoded one
  [
    "#######",
    "#*#***#",
    "**#*#*#",
    "#*#*#*#",
    "#***#**",
    "#*###*#",
    "#######"
  ].

check_next_move(0, 0, Map) ->
  io:format("1~n"),
  %% Search for the entry point that is on the left side of the maze.
  PossibleEntryPoints = [{[lists:nth(1, lists:nth(ValY, Map))], 1, ValY} || ValY <- lists:seq(1, length(Map))],
  io:format("~p~n",[PossibleEntryPoints]),
  [{"*", 1, NewY} || {Value, _, NewY} <- PossibleEntryPoints, Value == "*"];
check_next_move(_, Y, Map) when Y == length(Map) ->
  %% The last line is lower wall,
  %% normally our position should never be equal to that one
  io:format("2~n"),
  throw("Crashed in to the wall!!!");
check_next_move(X, Y, Map) ->
  io:format("3~n"),
  %check_next_move(X, Y, Map, length(lists:nth(1, Map))).%,
  Forward_cell = {[lists:nth(X + 1, lists:nth(Y, Map))], X + 1, Y},
  Up_cell = {[lists:nth(X, lists:nth(Y - 1, Map))], X, Y - 1},
  Down_cell = {[lists:nth(X, lists:nth(Y + 1, Map))], X, Y + 1},
  io:format("~n~p~n~p~n~p~n", [Forward_cell, Down_cell, Up_cell]),
  Check_for_finish = fun(Val, PosX, PosY, Maze) ->
    case PosX == length(lists:nth(1, Maze)) andalso Val == "*" of
    true ->
      {finish, PosX, PosY};
    false ->
      {Val, PosX, PosY}
    end
  end,
  [Check_for_finish(Value, XPos, YPos, Map)
    || _ = {Value, XPos, YPos} <- [Forward_cell, Up_cell, Down_cell], Value == "*"].

%check_next_move(X, Y, _Map, MapWidth) when X == MapWidth ->
%  io:format("3_1~n"),
%  [{finish, X, Y}];
%check_next_move(X, Y, Map, _MapWidth) ->
%  io:format("3_2~n"),
%  Forward_cell = {[lists:nth(X + 1, lists:nth(Y, Map))], X + 1, Y},
%  Up_cell = {[lists:nth(X, lists:nth(Y - 1, Map))], X, Y - 1},
%  Down_cell = {[lists:nth(X, lists:nth(Y + 1, Map))], X, Y + 1},
%  io:format("~n~p~n~p~n~p~n", [Forward_cell, Down_cell, Up_cell]),
%  [{Value, XPos, YPos}
%    || _ = {Value, XPos, YPos} <- [Forward_cell, Up_cell, Down_cell], Value == "*"].

spawn_scouts({_, X, Y}, Map, ParentPid, Path) ->
  io:format("Args: ~p~n~p~n~p~n~p~n~p~n", [X, Y, Map, ParentPid, Path]),
  ProcName = "scout_" ++ integer_to_list(erlang:unique_integer([positive])),
  io:format("ProcName: ~p~n", [ProcName]),
  NewP = gen_server:start_link({local, list_to_atom(ProcName)}, ?MODULE, [{X, Y, Map, ParentPid, [{X, Y} |Path]}], []),
  io:format("Spawned ~p~n", [NewP]).
