import PySimpleGUI as sg
import subprocess
import re

CELL_SIZE = 20
WALL_COLOR = "#2e2e2e"
PATH_COLOR = "#ffffff"
START_COLOR = "#2ecc71"
GOAL_COLOR = "#e74c3c"
SOLUTION_COLOR = "#f1c40f"  # yellow overlay

MAX_VIEW_W = 950
MAX_VIEW_H = 520

# Where compiled Erlang .beam files live (adjust if needed)
BEAM_PATH = "."  # or "ebin"


def load_maze(path):
    with open(path, "r") as f:
        lines = [line.rstrip("\n") for line in f]
    lines = [ln for ln in lines if ln.strip() != ""]
    if not lines:
        raise ValueError("Maze file is empty.")
    width = max(len(ln) for ln in lines)
    return [list(ln.ljust(width, "#")) for ln in lines]


def compile_erlang():
    subprocess.run(["erlc", "*.erl"], shell=True, check=True)


def solve_with_erlang(map_path: str, beam_path: str = "."):
    expr = f'solve_cli:run("{map_path}").'
    cmd = ["erl", "-noshell", "-pa", beam_path, "-eval", expr, "-s", "init", "stop"]

    p = subprocess.run(cmd, text=True, capture_output=True)
    if p.returncode != 0:
        raise RuntimeError(f"Erlang failed:\nSTDOUT:\n{p.stdout}\nSTDERR:\n{p.stderr}")

    out = p.stdout.strip()
    if out in ("", "no_solution"):
        return None

    return [(int(x), int(y)) for x, y in re.findall(r"\{(\d+),(\d+)\}", out)]


def draw_maze(graph: sg.Graph, maze):
    graph.erase()
    rows = len(maze)
    cols = len(maze[0]) if rows else 0

    for y in range(rows):
        for x in range(cols):
            cell = maze[y][x]
            if cell == "#":
                color = WALL_COLOR
            elif cell == "S":
                color = START_COLOR
            elif cell == "G":
                color = GOAL_COLOR
            else:
                color = PATH_COLOR  # includes '*'

            x0 = x * CELL_SIZE
            y0 = (rows - 1 - y) * CELL_SIZE
            x1 = x0 + CELL_SIZE
            y1 = y0 + CELL_SIZE

            graph.draw_rectangle((x0, y0), (x1, y1),
                                 fill_color=color, line_color="#444444")


def draw_solution_path(graph: sg.Graph, path, rows: int):
    """
    Draws solution path overlay on the graph.
    path is list of (x,y) in 1-based Erlang coordinates.
    """
    if not path:
        return

    for (x1b, y1b) in path:
        # Convert Erlang 1-based to Python 0-based cell indices
        x = x1b - 1
        y = y1b - 1

        # Convert cell indices to graph pixels (same as draw_maze)
        x0 = x * CELL_SIZE
        y0 = (rows - 1 - y) * CELL_SIZE
        x1 = x0 + CELL_SIZE
        y1 = y0 + CELL_SIZE

        # Draw a smaller rectangle on top so S/G colors still visible if you want
        pad = 4
        graph.draw_rectangle((x0 + pad, y0 + pad), (x1 - pad, y1 - pad),
                             fill_color=SOLUTION_COLOR, line_color=SOLUTION_COLOR)


def main():
    sg.theme("DarkGrey13")

    graph = sg.Graph(
        canvas_size=(MAX_VIEW_W, MAX_VIEW_H),
        graph_bottom_left=(0, 0),
        graph_top_right=(MAX_VIEW_W, MAX_VIEW_H),
        background_color="#1f1f1f",
        key="-GRAPH-",
    )

    maze_view = sg.Column(
        [[graph]],
        key="-MAZE_VIEW-",
        scrollable=True,
        vertical_scroll_only=False,
        size=(MAX_VIEW_W + 20, MAX_VIEW_H + 20),
        expand_x=False,
        expand_y=False
    )

    layout = [
        [sg.Text("Maze Solver GUI", font=("Arial", 16))],
        [sg.Input(key="-FILE-", expand_x=True),
         sg.FileBrowse(file_types=(("Text Files", "*.txt"),))],
        [sg.Button("Load Maze"), sg.Button("Exit")],
        [sg.Frame("Maze", [[maze_view]])]
    ]

    window = sg.Window("Maze Solver", layout, finalize=True, resizable=True)

    while True:
        event, values = window.read()
        if event in (sg.WINDOW_CLOSED, "Exit"):
            break

        if event == "Load Maze":
            try:
                file_path = values["-FILE-"]
                if not file_path:
                    raise ValueError("Please select a maze file first.")

                maze = load_maze(file_path)
                rows = len(maze)
                cols = len(maze[0])

                maze_w = cols * CELL_SIZE
                maze_h = rows * CELL_SIZE

                view_w = min(MAX_VIEW_W, maze_w)
                view_h = min(MAX_VIEW_H, maze_h)

                new_graph = sg.Graph(
                    canvas_size=(view_w, view_h),
                    graph_bottom_left=(0, 0),
                    graph_top_right=(maze_w, maze_h),
                    background_color="#1f1f1f",
                    key="-GRAPH-",
                )

                window["-MAZE_VIEW-"].update([[]])
                window.extend_layout(window["-MAZE_VIEW-"], [[new_graph]])
                window.refresh()

                # Draw maze
                draw_maze(window["-GRAPH-"], maze)

                compile_erlang()

                # Solve with Erlang and draw path
                path = solve_with_erlang(file_path, BEAM_PATH)
                if path is None:
                    sg.popup("No solution found.")
                else:
                    draw_solution_path(window["-GRAPH-"], path, rows)

            except subprocess.CalledProcessError as e:
                sg.popup_error(f"Erlang solver failed:\n{e}\n\nMake sure BEAM_PATH is correct: {BEAM_PATH}")
            except FileNotFoundError:
                sg.popup_error("Could not run 'erl'. Is Erlang installed and in your PATH?")
            except Exception as e:
                sg.popup_error(f"Failed to load/solve maze:\n{e}")

    window.close()


if __name__ == "__main__":
    main()
