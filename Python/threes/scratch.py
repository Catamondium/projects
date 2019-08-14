#!/usr/bin/env python3
from common import window
from functools import reduce, partial
from operator import eq
from deck import Card, Deck
from pprint import pprint

# 4 chain rule, applied to numbers
# gives us the indices of 4 chains in origonal list
lst = [0, 0, 0, 0, 2, 2, 1, 1, 1, 1]
idx = []
for i, win in enumerate(window(lst, n=4)):
    # if they're all equal to 1st
    # they're all equal to each other
    if all(map(partial(eq, win[0]), win)):
        idx.append(i)
print(idx)  # -> [0, 6]

# rising rank rule
# would operate on subsets between burns & resets
#  burns do consitute a run,
#  because the player can put down on nothing
#  However, resets must follow if ran
# as rising rank & suite rules don't apply
# TODO model suite rule


def predicate(a: Card, b: Card):
    return (a.rank == b.rank) or (a.suite == b.suite and a.rank < b.rank)


lst = [
    Card('2', 'Hearts'),
    Card('3', 'Hearts'),
    Card('3', 'Spades'),
    Card('A', 'Spades'),
    Card('J', 'Spades'),  # False, descended
    Card('Q', 'Hearts')  # False, changed rank and suite
]
pprint(list(window(lst)))
pprint([predicate(x, y) for x, y in window(lst)])
