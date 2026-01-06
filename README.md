# Maze Solver

An Erlang-based maze solver that explores a maze using multiple concurrent processes.

The solver separates responsibilities between:
- a **maze server** (holds the maze, start/goal positions, and global state)
- multiple **scout processes** (explore the maze concurrently)

---

## Features

- Concurrent maze exploration using Erlang processes
- Dynamic branching: new processes are spawned at junctions
- Global visited tracking to avoid duplicate exploration
- Supports arbitrary start (`S`) and goal (`G`) positions
- Text-based maze input

---

## Maze Format

The maze is provided as a plain text file where each character represents one cell.

### Symbols

- `#` – wall (not walkable)
- `*` – walkable path
- `S` – start position
- `G` – goal position

### Rules

- The maze must be a rectangular grid (all lines must have the same length).
- Exactly one `S` (start) and one `G` (goal) are recommended.
- **`S` and `G` can be placed anywhere in the maze**  
  (they do NOT need to be on the left/right sides).

### Example

```
#######
#*#***#
S*#*#*#
#*#*#*#
#***#*G
#*###*#
#######
```

---

## Movement Rules

- Movement is allowed only in the four cardinal directions:
  - up
  - down
  - left
  - right
- No diagonal movement.

---

## Getting Started (Erlang Shell)

### 1. Start Erlang

```bash
erl
```

### 2. Compile the modules

```erlang
c(maze_loader).
c(maze_server).
c(scout).
```

### 3. Start the maze server

This loads the maze and initializes global state:

```erlang
{ok, MazePid} = maze_server:start_link().
```

You will be prompted to enter the path to the maze file:

```
Enter the path of the file: /path/to/maze.txt
```

---

### 4. Start the solver (root scout)

Start the root scout process.  
Using the shell as the parent allows you to receive results directly.

```erlang
{ok, RootPid} = scout:start_link(self(), MazePid).
```

The solver starts exploring automatically.

---

### 5. Retrieve the result

When a path to the goal is found, the solver sends a message to the parent process.

In the Erlang shell, run:

```erlang
flush().
```

Expected output:

```erlang
{found, Path}
```

Where `Path` is a list of `{X, Y}` coordinates from `S` to `G`.

---

## Stopping the Solver

To stop the solver and all running scout processes:

```erlang
ok = scout:stop(RootPid).
```

To stop the maze server:

```erlang
ok = maze_server:stop(MazePid).
```

---

## Loading a Different Maze

Currently, the solver does not support hot-reloading of mazes.

To load a new maze:
1. Stop the current scout
2. Stop the current maze server
3. Start a new maze server
4. Start a new root scout

---

## Limitations (Current)

- The solver does **not guarantee the shortest path**.
  - The first found path is returned.
  - Global visited tracking may prune alternative shorter paths.
- There is no upper bound on the number of spawned scout processes.
- The search strategy is non-deterministic due to concurrency.
- The solver currently reports only one solution.

---

## Future Improvements

- Guarantee shortest path selection
- Prune branches using best-known path length
- Limit total number of scout processes
- GUI ↔ Erlang integration
- Visualization of the exploration process
