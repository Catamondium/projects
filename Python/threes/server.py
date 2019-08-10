#!/usr/bin/env python3
import socket
import socketserver
from common import *
from deck import Card, Deck, byRank
"""
https://en.wikipedia.org/wiki/Shithead_(card_game)
Currently unplayable, representation only
"""

""" rules TODO
turntaking (toggleable socket?)
game start:
    9 cards each initially (3 per hand)
    switch primaries and faceups
A is high
card draw (fill to 3) while rest exist
played pile may only rise in rank
    unless burned by 4 chain or 10
    or reset by 2
    ELSE pickup played pile to hand
chaining rules:
    rank rises for same suit
    suit changes for same rank
    ^^^^ either must hold throughout chain
held, faceup and faceown may only be accessed
in sequence as exhausted

test over Unix sockets?
    INET won't allow morethan 1 concurrent localhost conn
"""


class ThreesHandler(socketserver.BaseRequestHandler):
    def handle(self):
        pass


if __name__ == "__main__":
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument("players", type=int, help="Number of players to serve")
    parser.add_argument(
        "--mode", default='inet', choices=['inet', 'unix'], help="Network family")
    nspace = parser.parse_args()

    # Should probably be threading variants
    if nspace.mode == 'inet':
        server = socketserver.TCPServer(REMOTE, ThreesHandler)
    elif nspace.mode == 'unix':
        server = socketserver.UnixStreamServer(NIX, ThreesHandler)

    # How do we limit connections?
    with server:
        server.serve_forever()

    if nspace.mode == 'unix':
        from pathlib import Path
        Path(NIX).unlink()
