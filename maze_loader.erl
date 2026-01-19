-module(maze_loader).

-export([load/0, load/1]).

%% Interactive load (used in Erlang shell)
load() ->
  File = string:trim(io:get_line("Enter the path of the file: "), trailing, "\n"),
  {ok, BinaryData} = case File of
    [] ->
      file:read_file("maps/test_map.txt");
    _ ->
      file:read_file(File)
  end,
  Contents = binary_to_list(BinaryData),
  New = string:tokens(Contents, "\n"),
  New.

%% Non-interactive load (used by CLI / Python subprocess)
load(Path) when is_list(Path) ->
  load_from_path(Path).

load_from_path(Path) ->
  {ok, BinaryData} = file:read_file(Path),
  Contents = binary_to_list(BinaryData),
  %% split by newline; you can also trim \\r if needed later
  string:tokens(Contents, "\n").