#!/usr/bin/env python3
"""French deck playing card stuff"""
from collections import namedtuple
from random import shuffle
from itertools import takewhile
from functools import partial

Card = namedtuple('Card', ["rank", "suite"])
Card.__repr__ = lambda c: f"{c.rank} of {c.suite}"
Card.__doc__ = "Playing card representation"


def byRank(c: Card):
    """keyfunction, sort by card rank"""
    return Deck.ranks.index(c.rank)


def byBase(c: Card):
    """keyfunction, sort by position in Deck()"""
    return Deck()._cards.index(c)


class Deck:
    """Normal french deck representation"""
    ranks = [str(x) for x in range(2, 11)] + list("JKQA")
    suites = "Hearts Spades Diamonds Clubs".split(' ')

    def __init__(self, cards=None):
        """
        Create full standard deck if cards is None
        else create Deck containing cards
        """
        self._cards = cards or [Card(r, s)
                                for r in self.ranks
                                for s in self.suites]

    def __getitem__(self, i):
        return self._cards[i]

    def __setitem__(self, i, c):
        self._cards[i] = c

    def __delitem__(self, i):
        del self._cards[i]

    def __len__(self):
        return len(self._cards)

    def __repr__(self):
        return repr(self._cards)

    @staticmethod
    def deal(n, sample, deck=None):
        """
        Returns (decks, rest)
        where decks is n random Decks of size sample from deck
        and rest is the remaining cards from deck

        if deck is None takes from Deck()

        """
        cdeck = deck or Deck()
        shuffle(cdeck)
        out = []
        for _ in range(0, n):
            slc = slice(0, sample)
            out += Deck([cdeck[slc]])
            del cdeck[slc]
        rest = cdeck[::]
        return (out, Deck(rest))
