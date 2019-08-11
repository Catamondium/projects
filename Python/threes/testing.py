#!/usr/bin/env python3
import unittest
import deck
import common


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


class TestCommon(unittest.TestCase):
    def test_window(self):
        case = "ABCD"
        expected = [('A', 'B'), ('B', 'C'), ('C', 'D')]
        result = list(common.window(case))
        self.assertListEqual(expected, result)

    def test_call_iter(self):
        from io import StringIO
        case = StringIO("CALL ABC\nGALL DEF XYZ")
        expected = [('CALL', ['ABC']), ('GALL', ['DEF', 'XYZ'])]
        result = list(common.call_iter(case))
        self.assertListEqual(expected, result)


if __name__ == "__main__":
    unittest.main()
