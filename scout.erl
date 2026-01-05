-module(scout).
-behaviour(gen_server).

-export([start_link/2, start_child/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2, code_change/3, handle_info/2]).

-record(state, {
  x_y_cord :: {integer(), integer()},
  parent :: pid(),
  maze_pid :: pid(),
  path :: [{integer(), integer()}],
  monitored :: [{pid(), reference()}]
}).


%%% ===== Public API =====

start_link(ParentPid, MazePid) ->
    gen_server:start_link(?MODULE, {root, ParentPid, MazePid}, []).

start_child(ParentPid, MazePid, Pos, Path) ->
    gen_server:start_link(?MODULE, {child, ParentPid, MazePid, Pos, Path}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).


%%% ===== gen_server callbacks =====

init({root, ParentPid, MazePid}) ->
    process_flag(trap_exit, true),
    Start = maze_server:get_start(MazePid),
    gen_server:cast(self(), explore),
    dbg("INIT root start=~p parent=~p maze=~p", [Start, ParentPid, MazePid]),
    {ok, #state{x_y_cord=Start, parent=ParentPid, maze_pid=MazePid, path=[Start], monitored=[]}};
init({child, ParentPid, MazePid, Pos, Path}) ->
    process_flag(trap_exit, true),
    dbg("INIT child pos=~p parent=~p maze=~p path_len=~p", [Pos, ParentPid, MazePid, length(Path)]),
    {ok, #state{x_y_cord=Pos, parent=ParentPid, maze_pid=MazePid, path=Path, monitored=[]}}.

terminate(_Reason, _S=#state{monitored=Mon}) ->
  lists:foreach(fun({Pid,_}) -> catch gen_server:cast(Pid, stop) end, Mon),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

handle_cast(explore,
  S=#state{x_y_cord=Pos, maze_pid=MazePid, path=Path, monitored=Mon}) ->

  Goal = maze_server:get_goal(MazePid),
  dbg("EXPLORE pos=~p goal=~p path_len=~p children=~p", [Pos, Goal, length(Path), length(Mon)]),

  case Pos =:= Goal of
    true ->
      dbg("AT GOAL -> finish", []),
      gen_server:cast(self(), finish),
      {noreply, S};

    false ->
      Ns0 = maze_server:neighbors(MazePid, Pos),
      dbg("neighbors: ~p", [Ns0]),

      Ns1 = [N || N <- Ns0, not lists:member(N, Path)],
      dbg("after local-path filter: ~p", [Ns1]),

      %% Global visited gate
      Ns2 = [N || N <- Ns1, maze_server:visit(MazePid, N) =:= ok],
      dbg("after global-visit filter: ~p", [Ns2]),

      case Ns2 of
        [] ->
          dbg("DEAD_END at ~p (no moves)", [Pos]),
          gen_server:cast(self(), dead_end),
          {noreply, S};

        [Next] ->
          dbg("STEP -> ~p", [Next]),
          S2 = step_to(Next, S),
          gen_server:cast(self(), explore),
          {noreply, S2};

        [Next | Rest] ->
          dbg("BRANCH: keep=~p spawn=~p", [Next, Rest]),
          {NewMon, ChildPids} = spawn_children(Rest, S),
          dbg("spawned children: ~p", [ChildPids]),
          S2 = S#state{monitored = NewMon ++ Mon},
          S3 = step_to(Next, S2),
          gen_server:cast(self(), explore),
          {noreply, S3}
      end
  end;
handle_cast(dead_end, S=#state{monitored=Mon, x_y_cord=Pos}) ->
  case Mon of
    [] ->
      dbg("CAST dead_end at ~p and no children -> stopping", [Pos]),
      {stop, normal, S};
    _ ->
      dbg("CAST dead_end at ~p but children still running -> waiting", [Pos]),
      {noreply, S}
  end;
handle_cast(finish, S=#state{parent=Parent, path=Path, monitored=Mon}) ->
  PathFromStart = lists:reverse(Path),
  dbg("CAST finish path_len=~p -> notifying parent=~p and stopping children=~p",
      [length(PathFromStart), Parent, length(Mon)]),
  lists:foreach(fun({Pid,_}) -> catch gen_server:cast(Pid, stop) end, Mon),
  Parent ! {found, PathFromStart},
  {stop, normal, S};
handle_cast(stop, S=#state{monitored=Mon}) ->
  dbg("CAST stop -> stopping children=~p and exiting", [length(Mon)]),
  lists:foreach(fun({Pid,_}) -> catch gen_server:cast(Pid, stop) end, Mon),
  {stop, normal, S}.


handle_info({found, PathFromChild}, S=#state{parent=Parent, monitored=Mon}) ->
  dbg("INFO found from child! path_len=~p -> stop children=~p bubble to parent=~p",
      [length(PathFromChild), length(Mon), Parent]),
  lists:foreach(fun({Pid,_Ref}) -> catch gen_server:cast(Pid, stop) end, Mon),
  Parent ! {found, PathFromChild},
  {noreply, S};
handle_info({'DOWN', Ref, process, Pid, Reason}, S=#state{monitored=Mon, x_y_cord=Pos}) ->
  dbg("INFO DOWN pid=~p reason=~p", [Pid, Reason]),
  NewMon = [PR || PR={_P,R} <- Mon, R =/= Ref],
  {noreply, S#state{monitored=NewMon}};
handle_info({'EXIT', Pid, Reason}, S) ->
  dbg("INFO EXIT from linked pid=~p reason=~p", [Pid, Reason]),
  {noreply, S};
handle_info(Info, S) ->
  dbg("INFO other=~p", [Info]),
  {noreply, S}.



%%% ===== Internal helpers =====

dbg(Fmt, Args) ->
  io:format("~p scout(~p) " ++ Fmt ++ "~n", [erlang:monotonic_time(millisecond), self() | Args]).

step_to(NextPos, S=#state{path=Path}) ->
  S#state{x_y_cord=NextPos, path=[NextPos | Path]}.

spawn_children(PosList, _S=#state{maze_pid=MazePid, path=Path}) ->
  lists:foldl(
    fun(P, {AccMon, AccPids}) ->
      dbg("SPAWN child for pos=~p", [P]),
      {ok, Pid} = scout:start_child(self(), MazePid, P, [P | Path]),
      Ref = erlang:monitor(process, Pid),
      gen_server:cast(Pid, explore),
      {[{Pid, Ref} | AccMon], [Pid | AccPids]}
    end,
    {[], []},
    PosList
  ).

