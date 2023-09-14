-module(scout).
-behaviour(gen_server).
-record(state, {
  x_y_cord :: {integer(), integer()},
  map :: [string()],
  parent :: pid(),
  path :: [tuple()],
  monitored :: pid()
}).


-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1, code_change/3]).


%% ---------------------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------------------

-spec start_link(pid()) -> {ok, pid()}.
start_link(ParentPid) ->
  Maze_map = load_maze(),
  gen_server:start_link(
      {local, ?MODULE},
      ?MODULE,
      [{0, 0, Maze_map, ParentPid, [], []}],
      []
  ).

-spec stop(pid()) -> ok.
stop(Pid) ->
  io:format("STOP callled! Starting termination!"),
  gen_server:cast(Pid, stop).


%% ---------------------------------------------------------------------------
%% gen_server functions
%% ---------------------------------------------------------------------------

-spec init([{integer(), integer(), term(), pid(), list(), term()}]) -> {ok, #state{}}.
init([{PosX, PosY, MazeMap, ParentPid, Path, Monitored}]) ->
  gen_server:cast(self(), explore),
  {ok, #state{
    x_y_cord = {PosX, PosY},
    map = MazeMap,
    parent = ParentPid,
    path = Path,
    monitored = Monitored}
  }.

-spec handle_call(atom(), any(), #state{}) -> {reply, [{integer(), integer()}], #state{}}.
handle_call(status, _From, State) ->
  io:format("Current status is: ~p~n!", [lists:reverse(State#state.path)]),
  {reply, State#state.path, State};
handle_call(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

-spec handle_cast(atom(), #state{}) -> {noreply, #state{}}.
handle_cast(explore,
    #state{
      x_y_cord = {X, Y},
      map = Map,
      parent = _ParentPid,
      path = Path,
      monitored = Monitored
    } = State) ->

  Result = lists:filter(fun(R) -> path_filter(R, Path) end, check_next_move(X, Y, Map)),
  {Verdict, NewState} = case Result of
    [] ->
      {dead_end, State};
    [{finish, NewX, NewY}] ->
      NewS = State#state{x_y_cord = {NewX, NewY}},
      {finish, NewS};
    [{"*", NewX, NewY}] ->
      NewS = State#state{x_y_cord = {NewX, NewY}, path = [{NewX, NewY} | Path]},
      {explore, NewS};
    _ ->
      Refs = [spawn_scouts(R, Map, self(), Path) || {_, _NewX, _NewY}  = R <- Result],
      NewS = State#state{monitored = lists:append(Refs, Monitored)},
      {idle, NewS}
  end,
  gen_server:cast(self(), Verdict),
  {noreply, NewState};
handle_cast(idle, State) ->
  %% Process has spawned other processes to search path
  %% Waiting for the reply from spawned processes
  io:format("Going to idle state!~n~p~n", [State]),
  {noreply, State};
handle_cast(finish, State) ->
  %% The finish has been reached
  io:format("Exit reached!~n"),
  PathFromStart = lists:reverse(State#state.path),
  io:format("Path: ~p~n", [PathFromStart]),
  %gen_server:cast(State#state.parent, {finish, PathFromStart}),
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

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(Info, State) ->
  io:format("Info: ~p~n", [Info]),
  {noreply, State}.

-spec terminate(#state{}) -> ok.
terminate(State) ->
  %% Terminate process and his spawned processes.
  io:format("TERMINATING!~n"),
  lists:foreach(
    fun(MonitoredPid) ->
      gen_server:cast(MonitoredPid, stop),
      ok
    end,
    State#state.monitored
  ),
  ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_Old, State, _Additional) ->
  {ok, State}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

load_maze() ->
  %% To be implemented, for now return the hardcoded one
  maze_loader:load().

-spec path_filter({atom(), integer(), integer()}, [{integer(), integer()}]) -> boolean().
path_filter({finish, _X,_Y}, _Path) ->
  %% Exit from the maze detected
  true;
path_filter({_Val, X, Y}, Path) ->
  %% Filter out the positions that are
  %% already in Path to prevent walk in the circle
  not lists:member({X, Y}, Path).

-spec check_next_move(integer(), integer(), [string()]) -> [{atom(), integer(), integer()}].
check_next_move(0, 0, Map) ->
  %% Search for the entry point that is on the left side of the maze.
  PossibleEntryPoints = [{[lists:nth(1, lists:nth(ValY, Map))], 1, ValY} || ValY <- lists:seq(1, length(Map))],
  [{"*", 1, NewY} || {Value, _, NewY} <- PossibleEntryPoints, Value == "*"];
check_next_move(_, Y, Map) when Y == length(Map) ->
  %% The last line is lower wall,
  %% normally our position should never be equal to that one
  throw("Crashed in to the wall!!!");
check_next_move(X, Y, Map) ->
  check_next_move(X, Y, Map, length(lists:nth(1, Map))).

-spec check_next_move(integer(), integer(), [string()], integer()) -> [{atom(), integer(), integer()}].
check_next_move(X, Y, _Map, MapWidth) when X == MapWidth ->
  %% Reached the end of the maze.
  [{finish, X, Y}];
check_next_move(X, Y, Map, _MapWidth) ->
  %% Check the next 3 possible cells to move
  Forward_cell = {[lists:nth(X + 1, lists:nth(Y, Map))], X + 1, Y},
  Up_cell = {[lists:nth(X, lists:nth(Y - 1, Map))], X, Y - 1},
  Down_cell = {[lists:nth(X, lists:nth(Y + 1, Map))], X, Y + 1},
  [{Value, XPos, YPos}
    || _ = {Value, XPos, YPos} <- [Forward_cell, Up_cell, Down_cell], Value == "*"].

-spec spawn_scouts({_, integer(), integer()}, [string()], pid(), [{integer(), integer()}]) -> pid().
spawn_scouts({_, X, Y}, Map, ParentPid, Path) ->
  %% Create new processes and name them
  %% according to the X and Y positions
  ProcName = "scout_" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y),
  {ok, NewP} = gen_server:start_link(
    {local, list_to_atom(ProcName)},
    ?MODULE,
    [{X, Y, Map, ParentPid, [{X, Y} | Path], []}],
    []
  ),
  NewP.
