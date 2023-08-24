# maze_solver
A simple erlang program to find path in maze.
This Erlang application is designed to solve mazes by autonomously navigating through the maze from the entrance (left side) to the exit (right side). 
The maze solver follows the assumption that it can move only forward, up, and down. Walls are represented by the # symbol, and paths are represented by the * symbol.

Assumptions:
1. Movement Restrictions: The maze solver is only capable of moving forward, upward, and downward. It cannot move diagonally or backward.

2. Entrance and Exit: The maze entrance is assumed to be located on the left side of the maze, and the exit is assumed to be located on the right side.

3. Maze Representation: The maze is represented as a grid of characters where # represents walls, and * represents paths that the solver can traverse.

Getting Started:
To use the maze solver, follow these steps:

1. Compile the scout.erl file using Erlang's compiler.

2.Start the solver by calling the start_link/0 function in the scout module.
The solver will automatically start exploring the maze from the entrance to the exit based on the assumptions mentioned above.

To stop the solver, you can use the stop/0 function.

Customization:
The solver's behavior can be customized by modifying the code. If you wish to change the movement directions, entrance and exit locations, or maze representation, 
you can do so by adjusting the appropriate parts of the code in the scout module. 
Please note that this application is a simple demonstration of an autonomous maze solver and may not be optimal for all maze types.
