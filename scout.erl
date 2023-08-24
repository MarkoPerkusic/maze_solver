-module(scout).
-behaviour(gen_server).


-export([start_link/0, stop/0]).
-export([init/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ---------------------------------------------------------------------------
%% API functions
%% ---------------------------------------------------------------------------

start_link() ->
  gen_server:start_link(
    {
      {local, ?MODULE},
      ?MODULE,
      [0, 0],
      []
    }
  ).

stop() ->
  gen_server:cast(?MODULE, stop).


%% ---------------------------------------------------------------------------
%% gen_server functions
%% ---------------------------------------------------------------------------

init() ->
  {ok, {0, 0, load_maze()}}.

handle_call(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

handle_cast(stop, State) ->
  {stop, normal, State};
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