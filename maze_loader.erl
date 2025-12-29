-module(maze_loader).

-export([load/0]).

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
