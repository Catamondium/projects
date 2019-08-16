#!/usr/bin/env python3
import logging
"""
https://en.wikipedia.org/wiki/Shithead_(card_game)
Currently unplayable
"""
import socket
from common import *
from collections import defaultdict
from random import shuffle
from deck import Card, Deck, Hand, byRank, netToCard
from itertools import starmap, takewhile
from functools import partial

from threading import Thread, Barrier  # , Event
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
    suit can change for same rank
    ^ either must hold throughout chain
held, faceup and faceown may only be accessed
in sequence as exhausted
"""


class Task(Enum):
    MSG = auto()
    ENDGAME = auto()
    SWAP = auto()
    WAIT = auto()
    KILL = auto()


class Player(Thread):
    """client handler"""

    def __init__(self, hand: Hand, startbarrier, sock, addr=NIX):
        self.queue = Queue()
        self.hand = hand
        self.barrier = startbarrier
        self.sock = sock
        self.addr = addr
        super().__init__()

    def run(self):
        with self.sock.makefile(mode='rw') as conn:
            terminate = False
            while (not terminate):
                self.barrier.wait()
                task, *args = self.queue.get()

                msg = ""
                if task == Task.ENDGAME:
                    msg = f"{task.name} {args[0]}"
                    terminate = True
                elif task == Task.KILL:
                    msg = f"{task.name} {args[0]}"
                    terminate = True
                elif task == Task.MSG:
                    lines, msg = args
                    msg = f"{task.name} {lines}\n{msg}"
                elif task == Task.SWAP:
                    self._swap(conn)
                elif task == Task.WAIT:
                    self.barrier.wait()
                conn.write(msg + '\n')
                conn.flush()

    def showHand(self):
        """Display hand to user"""
        self.msg(str(self.hand))

    def swap(self):
        """Request user swaps faceups"""
        self.queue.put_nowait((Task.SWAP,))

    def _swap(self, conn):
        # NOTE, implementation doesn't have error conditions
        conn.write(Task.SWAP.name + '\n')
        conn.flush()
        stuff = conn.readline().strip().split(' ')
        cards = list(map(netToCard, stuff))
        froms, tos = cards[::2], cards[1::2]
        for tup in zip(froms, tos):
            self.hand.swap(*tup)  # you do stuff now though?
        conn.write(ACK + '\n')
        conn.flush()

    def msg(self, msg):
        """Send general information to user"""
        lines = len(msg.split('\n'))
        self.queue.put_nowait((Task.MSG, lines, msg))

    def sort(self):
        """Sort the hand"""
        self.hand.sort()

    def endgame(self, winner):
        """Declare game finished, and winner"""
        self.queue.put_nowait((Task.ENDGAME, winner))

    def kill(self, deceased=""):
        """Emergency shutdown"""
        self.queue.put_nowait((Task.KILL, deceased))

    def wait(self):
        self.queue.put_nowait((Task.WAIT,))


def broadcast(players, method, *args):
    """SEND to all players"""
    for player in players.values():
        method(player, *args)


def multicast(players, ks, method, *args, inclusive=True):
    """
    SEND to players in ks
    if inclusive==False, SEND to all except
    """
    if inclusive:
        def pred(k): return k not in ks
    else:
        def pred(k): return k in ks
    broadcast({k: v for k, v in players.items() if pred(k)}, method, *args)


def porter(players, event):
    """
    client handler exception daemon,
    signals event when any Player threads die,
    allowing survivors to shutdown gracefully
    """
    event.clear()
    while (all(map(Player.is_alive, players.values()))):
        pass
    event.set()


def gameloop(players):
    """Main controller"""
    #sigfault = Event()
    #pth = Thread(target=porter, args=(players, sigfault), daemon=True)
    # pth.start()
    for player in players.values():
        print(f"Conn on: {player.addr or NIX}")
    turnkeys = list(players.keys())
    shuffle(turnkeys)
    # just terminate game for now, after showing hand
    broadcast(players, Player.showHand)
    broadcast(players, Player.swap)
    broadcast(players, Player.showHand)
    broadcast(players, Player.endgame, None)
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
        "--local", action='store_const', default='inet', const='unix', help="Network over unix sockets")
    argv = parser.parse_args()

    server = socket.socket(trans_mode[argv.local][0])
    server.bind(*(trans_mode[argv.local][1]))
    try:
        bar = Barrier(argv.players)
        server.listen()
        players = dict()
        global playpile
        global graveyard
        graveyard = []
        hands, playpile = Deck.deal(argv.players, 9)
        hands = map(Hand, hands)

        for hand in hands:
            p = Player(hand, bar, *server.accept())
            p.start()
            players[p.ident] = p

        gameloop(players)
    finally:
        if argv.local == 'unix':
            from pathlib import Path
            Path(NIX).unlink()
