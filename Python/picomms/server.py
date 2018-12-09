#!/usr/bin/python3
# server reciever on RPi end

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
            print("%s\tconn@\t%s" % (time(), addr))
            while True:
                msg = conn.recv(3000)
                if msg.decode() != '':
                    print("%s:\t%s" % (
                        time(),
                        msg.decode().strip()))
                else:
                    break;
