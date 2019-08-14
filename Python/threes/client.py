#!/usr/bin/env python3
from collections import defaultdict
import socket
from common import trans_mode, AFFIRM
from sys import stdin
from deck import cardToNet, Card


def call_iter(it, sep=' '):
    """
    Adapt it to yield (name, *args) by word splitting
    Output is stripped of whitespace
    """
    for i in it:
        head, *tail = i.strip().split(sep)
        yield (head, tail)


handlers = dict()


def handler(func):
    """
    Decorator registering
    func as remote call handler
    """
    handlers[func.__name__.lower()] = func
    return func


def noop(*argv, **kw):
    pass


@handler
def msg(lines, reader=None, **kw):
    """Forward general information to player"""
    msg = str()
    for _ in range(int(lines)):
        msg += reader.readline()
    print(msg)


@handler
def endgame(winner, **kw):
    """
    Game end.
    Reciept of this call should imply subsequent EOF on socket
    terminating the client
    """
    print(f"{winner} won!")


def readcards():
    from itertools import starmap
    words = filter(lambda x: x != 'of', stdin.readline().split(' '))
    words = list(map(str.strip, words))
    ranks, suites = words[::2], words[1::2]
    return starmap(Card, zip(ranks, suites))


@handler
def swap(reader=None, sock=None, **kw):
    """Read in Cards for user faceup"""
    while(True):
        cards = readcards()
        netstr = ' '.join(map(cardToNet, cards))
        sock.sendall((netstr + '\n').encode('utf-8'))
        resp = reader.readline().strip()

        if resp == AFFIRM:
            break
        print("Try again")


def run_loop(sock):
    global started
    started = True
    reader = iter(sock.makefile())
    for name, argv in call_iter(reader):
        handlers.get(name.lower(), noop)(*argv, sock=sock, reader=reader)


if __name__ == "__main__":
    from argparse import ArgumentParser, ArgumentTypeError
    parser = ArgumentParser("Threes client")
    parser.add_argument(
        "--local", action='store_const', default='inet', const='unix', help="Network over unix sockets")
    argv = parser.parse_args()

    global started
    started = False
    client = socket.socket(trans_mode[argv.local][0])
    try:
        client.connect(*trans_mode[argv.local][1])
        run_loop(client)
    except FileNotFoundError:
        if started:
            pass
        else:
            print("ERROR: no local server running")
