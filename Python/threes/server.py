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
from enum import Enum, auto

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
    INET won't allow moterminatehan 1 concurrent localhost conn
"""


class Task(Enum):
    MSG = auto()
    ENDGAME = auto()


class Player(Thread):
    """client handler"""

    def __init__(self, x, hand, startbarrier, sock, addr=NIX):
        self.queue = Queue()
        self.id = x
        self.hand = hand
        self.barrier = startbarrier
        self.sock = sock
        self.addr = addr
        super().__init__()

    def __eq__(self, other):
        if isinstance(other, Player):
            oid = other.id
        elif isinstance(other, int):
            oid = other
        return self.id == oid

    def __hash__(self):
        return hash(self.id)

    def run(self):
        with self.sock.makefile() as f:
            reader = iter(f)
            terminate = False
            while (True):
                self.barrier.wait()
                task, *args = self.queue.get()

                if task == Task.ENDGAME:
                    msg = f"{task.name} {args[0]}"
                    terminate = True
                elif task == Task.MSG:
                    msg = f"{task.name} {args[0]}\n{args[1]}"

                self.sock.sendall((msg + '\n').encode('utf-8'))
                if terminate:
                    break

    def showHand(self):
        self.msg(str(self.hand))

    def msg(self, msg):
        lines = len(msg.split('\n'))
        self.queue.put_nowait((Task.MSG, lines, msg))

    def endgame(self, winner):
        self.queue.put_nowait((Task.ENDGAME, winner))


def broadcast(players, method, *args):
    """SEND to all players"""
    for player in players.values():
        method(player, *args)


def multicast(players, ks, method, *args, inclusive=True):
    """SEND to ks in players, if inclusive=False, send to all except"""
    if inclusive:
        def pred(k): return k not in ks
    else:
        def pred(k): return k in ks
    broadcast({k: v for k, v in players.items() if pred(k)}, method, *args)


def gameloop(players):
    """Main controller"""
    for player in players.values():
        print(f"Conn on: {player.addr or NIX}")
        player.start()
    turnkeys = list(players.keys())
    shuffle(turnkeys)

    # just terminate game for now, after showing hand
    broadcast(players, Player.showHand)
    broadcast(players, Player.endgame, "none")
    for player in players.values():
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
        "--local", default='inet', const='unix', help="Network over unix sockets")
    argv = parser.parse_args()

    server = socket.socket(trans_mode[argv.local][0])
    server.bind(*(trans_mode[argv.local][1]))
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
        if argv.local == 'unix':
            from pathlib import Path
            Path(NIX).unlink()
