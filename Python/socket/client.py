#!/usr/bin/python3
import socket

clidata = socket.gethostname(), 5007

with socket.socket() as client:
    client.connect(clidata)
    msg = client.recv(24)
print(f"Recieved:\t{msg.decode()}", end='')
