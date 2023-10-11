import socket
import PySimpleGUI as sg


def main():
    host = 'localhost'  # Change to the Erlang server's IP address or hostname if necessary
    port = 12345

    sg.theme('DarkAmber')
    layout = [[sg.Text('Some text on Row 1')],
              [sg.Text('Enter something on Row 2'), sg.InputText()],
              [sg.Button('Ok'), sg.Button('Cancel')]]

    # Create the Window
    window = sg.Window('Window Title', layout)

    # Event Loop to process "events" and get the "values" of the inputs
    while True:
        event, values = window.read()
        if event == sg.WIN_CLOSED or event == 'Cancel':  # if user closes window or clicks cancel
            break
        print('You entered ', values[0])

        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((host, port))
            message = "Hello from Python!"
            s.sendall(message.encode())
            data = s.recv(1024)

        print(f"Received: {data.decode()}")

    window.close()


if __name__ == "__main__":
    main()
