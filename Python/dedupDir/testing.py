#!/usr/bin/env python3
import unittest
import os
from pathlib import Path
from shutil import rmtree, copytree
from dedupDir import dedup


class TestDedup(unittest.TestCase):
    TARGET = Path('./dir')
    BACK = Path('./keepdir')

    def setUp(self):
        rmtree(self.TARGET, ignore_errors=True)
        copytree(self.BACK, self.TARGET)
        print(f"{self.TARGET} restored from {self.BACK}")

    def test_nonrecurse(self):
        self.assertEqual(dedup(self.TARGET, False), 0)

    def test_recurse(self):
        self.assertEqual(dedup(self.TARGET, True), 8)


if __name__ == "__main__":
    unittest.main()
