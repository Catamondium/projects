#!/usr/bin/env python3
import unittest
from mol import mass, sanitize, ElementError


class TestMass(unittest.TestCase):
    def test_zero(self):
        self.assertEqual(mass(''), 0)
        self.assertEqual(mass('5'), 0)

    def test_elem(self):
        self.assertEqual(mass('H'), 1)
        self.assertEqual(mass('5H'), 5)

    def test_nonelement(self):
        with self.assertRaises(ElementError):
            mass('Fee')

    def test_benzene(self):
        self.assertEqual(mass('C6H6'), 78.06)
        self.assertEqual(mass('5C6H6'), 5 * 78.06)

    def test_struct_dimethylpropane(self):
        struct = 'CH3(CH3)2CH3'
        self.assertEqual(mass(struct), 60.04)
        self.assertEqual(mass('5' + struct), 5 * 60.04)


class TestSanitize(unittest.TestCase):
    def test_empty(self):
        self.assertEqual(sanitize(''), '')

    def test_brackets(self):
        self.assertEqual(sanitize('(){}[]'), '()()()')

    def test_whitespace(self):
        self.assertEqual(sanitize(' \t\n'), '')

    def test_punct(self):
        self.assertEqual(sanitize(r"\"'`.<>!£$%&*+@~#¬_?^/\:;-="), '')


if __name__ == "__main__":
    unittest.main()
