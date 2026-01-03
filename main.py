import PySimpleGUI as sg

CELL_SIZE = 20
WALL_COLOR = "#2e2e2e"
PATH_COLOR = "#ffffff"

MAX_VIEW_W = 950   # maksimalna vidljiva širina u px
MAX_VIEW_H = 520   # maksimalna vidljiva visina u px

def load_maze(path):
    with open(path, "r") as f:
        lines = [line.rstrip("\n") for line in f]
    lines = [ln for ln in lines if ln.strip() != ""]
    if not lines:
        raise ValueError("Maze file is empty.")
    width = max(len(ln) for ln in lines)
    return [list(ln.ljust(width, "#")) for ln in lines]

def draw_maze(graph: sg.Graph, maze):
    graph.erase()
    rows = len(maze)
    cols = len(maze[0]) if rows else 0

    for y in range(rows):
        for x in range(cols):
            cell = maze[y][x]
            color = PATH_COLOR if cell == "*" else WALL_COLOR

            x0 = x * CELL_SIZE
            y0 = (rows - 1 - y) * CELL_SIZE
            x1 = x0 + CELL_SIZE
            y1 = y0 + CELL_SIZE

            graph.draw_rectangle((x0, y0), (x1, y1),
                                 fill_color=color, line_color="#444444")

def main():
    sg.theme("DarkGrey13")

    # placeholder graph, zamijenit ćemo ga kad učitamo mapu
    graph = sg.Graph(
        canvas_size=(MAX_VIEW_W, MAX_VIEW_H),
        graph_bottom_left=(0, 0),
        graph_top_right=(MAX_VIEW_W, MAX_VIEW_H),
        background_color="#1f1f1f",
        key="-GRAPH-",
    )

    # scrollable viewport za labirint
    maze_view = sg.Column(
        [[graph]],
        key="-MAZE_VIEW-",
        scrollable=True,
        vertical_scroll_only=False,
        size=(MAX_VIEW_W + 20, MAX_VIEW_H + 20),  # malo prostora za scroll bar
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
                maze = load_maze(values["-FILE-"])
                rows = len(maze)
                cols = len(maze[0])

                maze_w = cols * CELL_SIZE
                maze_h = rows * CELL_SIZE

                # vidljiva veličina ostaje ograničena, ali "virtualna" je veličina labirinta
                view_w = min(MAX_VIEW_W, maze_w)
                view_h = min(MAX_VIEW_H, maze_h)

                new_graph = sg.Graph(
                    canvas_size=(view_w, view_h),
                    graph_bottom_left=(0, 0),
                    graph_top_right=(maze_w, maze_h),
                    background_color="#1f1f1f",
                    key="-GRAPH-",
                )

                # zamijeni sadržaj u scrollable koloni
                window["-MAZE_VIEW-"].update([[]])
                window.extend_layout(window["-MAZE_VIEW-"], [[new_graph]])
                window.refresh()

                draw_maze(window["-GRAPH-"], maze)

            except Exception as e:
                sg.popup_error(f"Failed to load maze:\n{e}")

    window.close()

if __name__ == "__main__":
    main()

