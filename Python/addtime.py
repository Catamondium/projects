#!/usr/bin/env python3
# Python 3.6
import argparse


class Time:
    """hrs:mins simplistic time"""

    def __init__(self, hrs=0, mins=0):
        self.hrs = hrs  # becoming 1?
        self.mins = mins

    def __repr__(self):
        return f"{self.hrs:02d}:{self.mins:02d}"

    def __add__(self, t):
        """Return Time + mins t"""
        tot = t + abs(self)
        return Time.from_int(tot)

    def __abs__(self):
        """Returns the total minutes represented"""
        return (self.hrs * 60) + self.mins

    @classmethod
    def from_int(cls, i):
        return cls(i // 60, i % 60)

    @classmethod
    def from_string(cls, string):
        if ':' in string:
            parts = [int(x) for x in string.split(':')]
            return cls(parts[0], parts[1])
        else:
            return cls.from_int(int(string))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("Start", type=Time.from_string,
                        help="HH:MM or mins, representing beginning time")
    parser.add_argument("Elapse", type=Time.from_string,
                        help="HH:MM or mins, representing time to elapse by")
    parser.add_argument('-q', '--quiet', action='store_true',
                        help="Print elapsed time only")
    args = parser.parse_args()

    start = args.Start
    elapse = abs(args.Elapse)

    if args.quiet:
        print(f"{start + elapse}")
    else:
        print(f"Start:\t{start}\t{elapse:+}\nEnd:\t{start + elapse}")
