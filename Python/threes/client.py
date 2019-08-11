#!/usr/bin/env python3
from collections import defaultdict
import socket
from common import *

"""
Should client ever recieve a Hand?
    no, because facedown's musn't be seen
    just send a printing for stdout
"""


def noop(*argv, **kwargv):
    pass


_handlers = []
handlers = defaultdict(lambda: noop, _handlers)


def run_loop(sock):
    reader = iter(sock.makefile())
    for name, argv in call_iter(reader):
        handlers[name](*argv, sock=sock, reader=reader)


if __name__ == "__main__":
    from argparse import ArgumentParser, ArgumentTypeError
    parser = ArgumentParser("Threes client")
    parser.add_argument(
        "--mode", default='inet', choices=trans_mode.keys(), help="Network family")
    argv = parser.parse_args()

    client = socket.socket(trans_mode[argv.mode][0])
    client.connect(*trans_mode[argv.mode][1])
    run_loop(client)

    if argv.mode == 'unix':
        from pathlib import Path
        Path(NIX).unlink()
