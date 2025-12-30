# Maze Solver

A simple Erlang program to find a path in a maze.

## Features
- Autonomous maze exploration
- Supports only forward, up, and down moves
- Maze represented with `#` for walls and `*` for paths

## Assumptions
1. **Movement:** No diagonal or backward movement.
2. **Entrance & Exit:** Left side = entrance, right side = exit.
3. **Maze Representation:** Grid of characters (`#` walls, `*` paths).

## Getting Started

1. Compile the `scout.erl` file in Erlang shell:
   ```erlang
   c(scout).

2. Start the solver:
   ```erlang
   scout:start_link(self()).

Press Enter for default map (maps/test_map.txt)
Or enter a custom map path (maps/my_maze.txt)

3. Stop the solver:
   ```erlang
   scout:stop(whereis(scout)).

## Customization
   ```markdown
   #######
   #*#***#
   **#*#*#
   #*#*#*#
   #***#**
   #*###*#
   #######

## Example Path Found
   ```rust
   (0,1) -> (1,1) -> (2,1) -> ... -> (6,5)

