#!/usr/bin/env python3
"""
https://en.wikipedia.org/wiki/Shithead_(card_game)
Currently unplayable, representation only
"""
from deck import Deck, byRank


class Hand:
    def __init__(self, deal):
        assert(len(deal) == 9)
        self.held = sorted(deal[:3], key=byRank)
        self.faceup = sorted(deal[3:6], key=byRank)
        self.facedown = deal[6:]
        self.hidden = True

    def __repr__(self):
        return f"Hand(\n\theld    :\t{self.held}\n\tfaceup  :\t{self.faceup}\n\tfacedown:\t[ {len(self.facedown)} cards ])"


if __name__ == "__main__":
    from pprint import pprint
    nplayers = 5
    d, takepile = Deck().deal(nplayers, 3*3)
    players = []
    for x in range(nplayers):
        players.append(Hand(d[x]))
    pprint(players)
