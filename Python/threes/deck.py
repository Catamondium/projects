#!/usr/bin/env python3
"""French deck playing card stuff"""
from collections import namedtuple
from random import shuffle


class Rank:
    """Richly comparable rank type"""

    def __init__(self, value):
        if value not in Deck.ranks:
            raise TypeError(f"{value} is not a rank")
        self.value = value

    def __eq__(self, other):
        return self.value == other.value

    def __lt__(self, other):
        return Deck.ranks.index(self.value) < Deck.ranks.index(other.value)

    def __repr__(self):
        return self.value


class Card:
    "Playing card"

    def __init__(self, rank, suite):

        if suite not in Deck.suites:
            raise TypeError(f"{suite} is not a suite")

        self.rank = Rank(rank)
        self.suite = suite

    def __eq__(self, other):
        return (self.rank == other.rank) and (self.suite == other.suite)

    def __hash__(self):
        return hash((self.rank, self.suite))

    def __repr__(self):
        return f"{self.rank} of {self.suite}"


def cardToNet(c):
    """Put card into 'word' form"""
    return f"{c.rank}:{c.suite}"


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


class Hand:
    def __init__(self, deal):
        assert(len(deal) == 9)
        self.held = sorted(deal[:3], key=byRank)
        self.faceup = sorted(deal[3:6], key=byRank)
        self.facedown = deal[6:]
        self.hidden = True

    def __repr__(self):
        return f"held    :\t{self.held}\nfaceup  :\t{self.faceup}\nfacedown:\t[ {len(self.facedown)} cards ]"
