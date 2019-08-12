#!/usr/bin/env python3
from collections import defaultdict
import socket
from common import *

"""
Should the client ever recieve a Hand?
    no, because facedown's musn't be seen
    just send a printing for stdout
"""


handlers = defaultdict(lambda: noop)


def handler(func):
    handlers[func.__name__.lower()] = func
    return func


def noop(*argv, **kwargs):
    pass


@handler
def endgame(winner, **kwargs):
    """
    Game end.
    Reciept of this call should imply subsequent EOF on reader
    terminating the client
    """
    print(f"{winner} won!")


def run_loop(sock):
    global started
    started = True
    reader = iter(sock.makefile())
    for name, argv in call_iter(reader):
        handlers[name.lower()](*argv, sock=sock, reader=reader)


if __name__ == "__main__":
    from argparse import ArgumentParser, ArgumentTypeError
    parser = ArgumentParser("Threes client")
    parser.add_argument(
        "--mode", default='inet', choices=trans_mode.keys(), help="Network family")
    argv = parser.parse_args()

    global started
    started = False
    client = socket.socket(trans_mode[argv.mode][0])
    try:
        client.connect(*trans_mode[argv.mode][1])
        run_loop(client)
    except FileNotFoundError:
        if started:
            pass
        else:
            print("ERROR: no local server running")
