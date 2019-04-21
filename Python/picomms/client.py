#!/usr/bin/python3
# client controller on user end
# project cancelled, lacking resources

import socket
import sys

# sub hostname for static RPi addr
clidata = socket.gethostname(), 5007


def quit(conn, args):
    conn.shutdown(socket.SHUT_RDWR)
    conn.close()
    sys.exit()


handlers = {
    "quit": quit
}


def handle(conn, string):
    stuff = string.split()
    if stuff[0] in handlers:
        handlers[stuff[0]](conn, stuff[1:])
    else:
        conn.send(string.encode())


with socket.socket() as client:
    client.connect(clidata)
    while True:
        handle(client, input(":>\t"))
