#!/usr/bin/python3
# client controller on user end

import socket
import sys

# sub hostname for static RPi addr 
clidata = socket.gethostname(), 5007

def handle(string, conn):
    if(string == "quit"):
        conn.shutdown(socket.SHUT_RDWR)
        conn.close()
        sys.exit()
    else:
        conn.send(string.encode())

with socket.socket() as client:
    client.connect(clidata)
    while True:
        handle(input(":>\t"), client)
