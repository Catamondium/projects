#!/usr/bin/python3
# server reciever on RPi end
# project cancelled, lacking resources

import socket
import math
from time import localtime, strftime

servdata = socket.gethostname(), 5007


def time():
    return strftime("%d/%m/%y %H:%M", localtime())


with socket.socket() as server:
    # options to allow socket reuse after interrupt
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(servdata)

    print("Listening...")
    while True:
        server.listen(1)
        conn, addr = server.accept()
        with conn:
            addr = "%s:%s" % addr
            print(f"{time()}\tconn@\t{addr}")
            while True:
                msg = conn.recv(3000)
                if msg.decode() != '':
                    print("{}:\t{}".format(
                        time(),
                        msg.decode().strip()))
                else:
                    break
