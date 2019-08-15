#!/usr/bin/env python3
import unittest
import deck
import common
import client


class TestClient(unittest.TestCase):
    def test_call_iter(self):
        from io import StringIO
        case = StringIO("CALL ABC\nGALL DEF XYZ")
        expected = [('CALL', ['ABC']), ('GALL', ['DEF', 'XYZ'])]
        result = list(client.call_iter(case))
        self.assertListEqual(expected, result)


class TestServer(unittest.TestCase):
    pass


class TestDeck(unittest.TestCase):
    def test_deal(self):
        n = 5
        sample = 9
        fullsize = len(deck.Deck())
        deals, rest = deck.Deck.deal(n, sample)

        self.assertEqual(len(deals), n, f"Failed to produce {n} deals")
        map(lambda l: self.assertEqual(len(l), sample), deals)
        self.assertEqual(len(rest), fullsize - (n * sample))

        deals += [rest]
        flat = deck.Deck([i for sublist in deals for i in sublist])
        self.assertEqual(len(flat), len(deck.Deck()),
                         "Failed to use entire deck")

        flat = deck.Deck(sorted(flat, key=deck.byBase))
        self.assertListEqual(flat._cards, deck.Deck()._cards, "Modified list")


class TestHand(unittest.TestCase):
    def test_swap(self):
        from itertools import repeat
        A = deck.Card('a', 'spades')
        B = deck.Card('j', 'hearts')
        C = deck.Card('k', 'clubs')

        held = list(repeat(A, 3))
        faceup = list(repeat(B, 3))
        facedown = list(repeat(C, 3))

        hand = deck.Hand(held + faceup + facedown)
        hand.swap(A, B)
        self.assertIn(B, hand.held)
        self.assertIn(A, hand.faceup)


class TestCommon(unittest.TestCase):
    def test_window(self):
        case = "ABCD"
        expected = [('A', 'B'), ('B', 'C'), ('C', 'D')]
        result = list(common.window(case))
        self.assertListEqual(expected, result)


if __name__ == "__main__":
    unittest.main()
