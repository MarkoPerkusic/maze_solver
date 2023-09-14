-module(maze_loader).

-export([load/0]).

load() ->
  io:format("Enter the path of the file: ~n"),
  File = string:strip(io:get_line(""), right, $\n),
  {ok, BinaryData} = file:read_file(File),
  Contents = binary_to_list(BinaryData),
  New = string:tokens(Contents, "\n"),
  New.