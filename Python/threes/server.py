#!/usr/bin/env python3
"""
https://en.wikipedia.org/wiki/Shithead_(card_game)
Currently unplayable
"""
import socket
from common import *
from collections import defaultdict
from random import shuffle
from deck import Card, Deck, Hand, byRank

from threading import Thread, Barrier
from queue import Queue

""" rules TODO
turntaking
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


class Player(Thread):
    """client handler"""

    def __init__(self, x, hand, startbarrier, sock, addr=None):
        self.queue = Queue()
        self.id = x
        self.hand = hand
        self.barrier = startbarrier
        self.sock = sock
        self.addr = addr
        super().__init__()

    def run(self):
        with self.sock.makefile() as f:
            reader = iter(f)
            while (True):
                self.barrier.wait()
                task = self.queue.get()  # blocking get
                self.sock.sendall((task + '\n').encode('utf-8'))
                if task[:7] == "ENDGAME":
                    break


def broadcast(msg, players):
    """SEND to all players"""
    for player in players:
        player.queue.put_nowait(msg)


def multicast(msg, players, xs, inclusive=True):
    """SEND to xs in players, if inclusive=False, send to all except"""
    if inclusive:
        def pred(x): return x in xs
    else:
        def pred(x): return x not in xs

    for k, player in players:
        if pred(k):
            player.queue.put_nowait(msg)


"""
When purely string messaging gets silly
We should probably use Player methods and
a struct with enums
"""


def gameloop(players):
    """Main controller"""
    for player in players:
        print(f"Conn on: {player.addr or trans_mode['unix'][1]}")
        player.start()
    turnkeys = players.keys()
    shuffle(turnkeys)

    # just terminate game for now, after showing hand
    for _, p in players:
        s = str(p.hand)
        lines = len(s.split('\n'))
        p.queue.put_nowait(f"MSG {lines}\n{s}")
    broadcast("ENDGAME none", players)
    for player in players:
        player.join()


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
    server.bind(*(trans_mode[argv.mode][1]))
    try:
        bar = Barrier(argv.players)
        server.listen()
        players = dict()
        global playpile
        global graveyard
        hands, playpile = Deck.deal(argv.players, 9)
        hands = list(map(Hand, hands))

        for x in range(argv.players):
            players[x] = Player(x, hands[x], bar, *server.accept())

        gameloop(players)
    finally:
        if argv.mode == 'unix':
            from pathlib import Path
            Path(NIX).unlink()
