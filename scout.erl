-module(scout).
-behaviour(gen_server).
-record(state, { x_y_cord, map, parent, path}).


-export([start_link/1, stop/1, show_progress/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ---------------------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------------------

start_link(ParentPid) ->
  Maze_map = load_maze(),
  io:format("ParentPid ~p~n", [ParentPid]),
  gen_server:start_link(
      {local, ?MODULE},
      ?MODULE,
      [{1, 1, Maze_map, ParentPid, []}],
      []
  ).

stop(Pid) ->
  gen_server:cast(Pid, stop).

show_progress() ->
  gen_server:call(?MODULE, status).


%% ---------------------------------------------------------------------------
%% gen_server functions
%% ---------------------------------------------------------------------------

init([{PosX, PosY, MazeMap, ParentPid, Path}]) ->
  gen_server:cast(?MODULE, explore),
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
      parent = ParentPid,
      path = Path
    } = State) ->
  io:format("Exploring!"),
  Result = check_next_move(X, Y, Map),
  io:format("Result: ~p~n", [Result]),
  _Verdict = case Result of
    [] ->
      dead_end;
    [{finish, _NewX, _NewY}] ->
      finish;
    [{_Val, _NewX, _NewY}] ->
      explore;
    _ ->
      [spawn_scouts(R, Map, ParentPid, Path) || R <- Result],
      idle %%TODO implement handler for this case
  end,
  X, Y, _ = Result,
  NewPath = [{X, Y} | State#state.path],
  %% gen_server:cast(?MODULE, Verdict)
  {noreply, State#state{path = NewPath}};
handle_cast(finish, State) ->
  %% The finish has been reached
  io:format("Exit reached!"),
  {noreply, State};
handle_cast(dead_end, State) ->
  %% The spawned process has reached dead end
  io:format("Dead end reached!"),
  {noreply, State};
handle_cast(stop, State) ->
  %% Terminate
  io:format("Calling terminate!"),
  terminate(stop, State);
handle_cast(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  error_logger:info_msg("Info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Info, _State) ->
  io:format("TERMINATING!~n"),
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
      {finish, PosX, PosY};
    false ->
      {Val, PosX, PosY}
    end
  end,
  [Check_for_finish(Value, XPos, YPos, Map)
    || _ = {Value, XPos, YPos} <- [Forward_cell, Up_cell, Down_cell], Value == "*"].


spawn_scouts({_, X, Y}, Map, ParentPid, Path) ->
  NewP = gen_server:start_link({local, ?MODULE}, ?MODULE, [{X, Y, Map, ParentPid, Path}], []),
  io:format("Spawned ~p~n", [NewP]).