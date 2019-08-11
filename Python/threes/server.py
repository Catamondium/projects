#!/usr/bin/env python3
import socket
from common import *
from collections import defaultdict
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
    ^ either must hold throughout chain
held, faceup and faceown may only be accessed
in sequence as exhausted

test over Unix sockets?
    INET won't allow morethan 1 concurrent localhost conn
"""


def serve(x, sock, addr):
    pass


if __name__ == "__main__":
    from argparse import ArgumentParser, ArgumentTypeError

    def player_type(i):
        x = int(i)
        if not x > 1:
            raise ArgumentTypeError("Must have more than 1 player.")
        return x

    parser = ArgumentParser("Threes server")
    parser.add_argument("players", type=player_type,
                        help="Number of players to serve")
    parser.add_argument(
        "--mode", default='inet', choices=trans_mode.keys(), help="Network family")
    argv = parser.parse_args()

    server = socket.socket(trans_mode[argv.mode][0])
    server.bind(*trans_mode[argv.mode][1])
    for x in range(argv.players):
        serve(x, *server.accept())

    if argv.mode == 'unix':
        from pathlib import Path
        Path(NIX).unlink()
