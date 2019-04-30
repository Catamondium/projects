#!/usr/bin/env python3
# Python 3.6
import sys


class Time:
    """hrs:mins simplistic time"""

    def __init__(self, hrs, mins):
        self.hrs = hrs
        self.mins = mins

    def __repr__(self):
        return f"{self.hrs:02d}:{self.hrs:02d}"

    def __add__(self, t):
        """Add a pair of Time objects"""
        tot = t + abs(self)
        return Time(tot // 60, tot % 60)

    def __abs__(self):
        """Returns the total minutes represented"""
        return (self.hrs * 60) + self.mins


def pTime(string):
    Istr = sys.argv[1].split(':')
    parts = [int(x) for x in Istr]
    return Time(parts[0], parts[1])


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Error:\thh:mm mins expected")
        sys.exit(1)

    start = pTime(sys.argv[1])

    if ":" in sys.argv[2]:
        elapse = abs(pTime(sys.argv[2]))
    else:
        elapse = int(sys.argv[2])

    print(f"Start:\t{start}\t{elapse:+}\nEnd:\t{start + elapse}")
