#!/usr/bin/env python3
import unittest
import os
from pathlib import Path
from shutil import rmtree, copytree
from dedupDir import dedup
from typing import Set


class TestDedup(unittest.TestCase):
    TARGET = Path('/dev/shm/dir')
    BACK = Path('./backup')

    def setUp(self):
        rmtree(self.TARGET, ignore_errors=True)
        copytree(self.BACK, self.TARGET)

    def test_nonrecurse(self):
        """Successful flat dedup"""
        result = dedup(self.TARGET, False)
        expected = set()
        self.assertEqual(result, expected)

    def test_recurse(self):
        """Successfully recurses"""
        result = dedup(self.TARGET, True)
        expected = 8
        self.assertEqual(len(result), expected)
        self.assertTrue(seqcheck(result))


def seqcheck(dels: Set[Path]):
    """We should have 1...7 among the deleted"""
    names = sorted([x.name for x in dels])
    nums = (str(x) for x in range(8))
    return all(y in x for (x, y) in zip(names, nums))


if __name__ == "__main__":
    unittest.main()
