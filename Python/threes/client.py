#!/usr/bin/env python3
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