#!/usr/bin/python3
import socket
import math

# very simple server
servdata = socket.gethostname(), 5007
msg = "PI = %s\r\n" % (math.pi)

with socket.socket() as server:
    # options to allow socket reuse after interrupt
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(servdata)

    print("Listening...")
    while True:
        server.listen(1)
        conn, addr = server.accept()
        with conn:
            print("Connected@\t%s:%s" % addr)
            conn.send(msg.encode())
            conn.close()
