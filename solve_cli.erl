-module(solve_cli).
-export([run/1]).

run(Path) when is_list(Path) ->
    {ok, MazePid} = maze_server:start_link(Path),
    {ok, _RootPid} = scout:start_link(self(), MazePid),
    receive
        {found, SolPath} -> io:format("~p~n", [SolPath])
    after 30000 ->
        io:format("no_solution~n")
    end,
    ok = maze_server:stop(MazePid),
    ok.
