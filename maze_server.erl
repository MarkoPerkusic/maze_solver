%%%-------------------------------------------------------------------
%%% maze_server.erl
%%% Owns:
%%%   - the grid loaded once via maze_loader:load/0
%%%   - global visited set
%%%   - optional stored solution
%%%
%%% Grid format:
%%%   Grid = [RowString, RowString, ...]
%%% Coordinates:
%%%   Pos = {X, Y} where X is column (1..W), Y is row (1..H)
%%%
%%% Assumptions about map symbols:
%%%   'S' = start
%%%   'G' = goal
%%%   '#' = wall
%%%   anything else = walkable
%%%-------------------------------------------------------------------
-module(maze_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/1]).
-export([get_start/1, get_goal/1, neighbors/2,
         visit/2, set_solution/2, get_solution/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    grid,              % [string()]
    width,             % integer()
    height,            % integer()
    start,             % {X,Y}
    goal,              % {X,Y}
    visited,           % sets:set()
    solution = undefined
}).

%%% ===== Public API =====

%% Starts and loads a maze by prompting the user (via maze_loader:load/0)
start_link() ->
    gen_server:start_link(?MODULE, noargs, []).
start_link(Path) ->
    gen_server:start_link(?MODULE, {from_file, Path}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

get_start(Pid) ->
    gen_server:call(Pid, get_start).

get_goal(Pid) ->
    gen_server:call(Pid, get_goal).

neighbors(Pid, Pos) ->
    gen_server:call(Pid, {neighbors, Pos}).

%% Global visited: returns ok | already
visit(Pid, Pos) ->
    gen_server:call(Pid, {visit, Pos}).

%% Store a solution only once: returns ok | already
set_solution(Pid, Path) ->
    gen_server:call(Pid, {set_solution, Path}).

get_solution(Pid) ->
    gen_server:call(Pid, get_solution).


%%% ===== gen_server callbacks =====

init(noargs) ->
    Grid0 = maze_loader:load(),
    init_from_grid(Grid0);

init({from_file, Path}) ->
    Grid0 = maze_loader:load(Path),
    init_from_grid(Grid0).

handle_call(get_start, _From, S=#state{start=Start}) ->
    {reply, Start, S};

handle_call(get_goal, _From, S=#state{goal=Goal}) ->
    {reply, Goal, S};

handle_call({neighbors, Pos}, _From, S) ->
    Ns = get_neighbors(Pos, S),
    {reply, Ns, S};

handle_call({visit, Pos}, _From, S=#state{visited=Vis}) ->
    case sets:is_element(Pos, Vis) of
        true ->
            {reply, explored, S};
        false ->
            Vis2 = sets:add_element(Pos, Vis),
            {reply, ok, S#state{visited=Vis2}}
    end;

handle_call({set_solution, Path}, _From, S=#state{solution=undefined}) ->
    {reply, ok, S#state{solution=Path}};
handle_call({set_solution, _Path}, _From, S=#state{solution=_Exist}) ->
    {reply, already, S};

handle_call(get_solution, _From, S=#state{solution=Sol}) ->
    {reply, Sol, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(Other, _From, S) ->
    {reply, {error, {unknown_call, Other}}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%% ===== Internal helpers =====

init_from_grid(Grid0) ->
    process_flag(trap_exit, true),
    Grid = normalize_grid(Grid0),
    H = length(Grid),
    W = case Grid of [] -> 0; [R|_] -> length(R) end,

    Start = find_char(Grid, $S),
    Goal  = find_char(Grid, $G),

    case {Start, Goal} of
        {undefined, _} -> {stop, {error, no_start_found}};
        {_, undefined} -> {stop, {error, no_goal_found}};
        {SPos, GPos} ->
            Vis0 = sets:new(),
            Vis1 = sets:add_element(SPos, Vis0),
            {ok, #state{
                grid=Grid, width=W, height=H,
                start=SPos, goal=GPos,
                visited=Vis1
            }}
    end.

%% Ensure all rows are the same width (pad with spaces if needed)
normalize_grid(Grid) ->
    W = max_width(Grid),
    [ pad_row(Row, W) || Row <- Grid ].

max_width([]) -> 0;
max_width(Rows) ->
    lists:max([length(R) || R <- Rows]).

pad_row(Row, W) ->
    case length(Row) of
        Len when Len >= W -> Row;
        Len ->
            Row ++ lists:duplicate(W - Len, $\s)
    end.

%% Find the first occurrence of a character in the grid; return {X,Y} or undefined
find_char(Grid, Char) ->
    find_char(Grid, Char, 1).

find_char([], _Char, _Y) ->
    undefined;
find_char([Row|Rest], Char, Y) ->
    case find_in_row(Row, Char, 1) of
        undefined -> find_char(Rest, Char, Y + 1);
        X -> {X, Y}
    end.

find_in_row([], _Char, _X) ->
    undefined;
find_in_row([Char|_], Char, X) ->
    X;
find_in_row([_|T], Char, X) ->
    find_in_row(T, Char, X + 1).

get_neighbors({X,Y}, S=#state{width=W, height=H}) ->
    Candidates = [{X, Y-1}, {X, Y+1}, {X-1, Y}, {X+1, Y}],
    [P || P <- Candidates, in_bounds(P, W, H), is_walkable(P, S)].

in_bounds({X,Y}, W, H) ->
    X >= 1 andalso X =< W andalso Y >= 1 andalso Y =< H.

is_walkable({X,Y}, #state{grid=Grid}) ->
    C = cell(Grid, X, Y),
    %% '#' is a wall, everything else is walkable (including 'S' and 'G')
    C =/= $#.

cell(Grid, X, Y) ->
    Row = lists:nth(Y, Grid),
    lists:nth(X, Row).

