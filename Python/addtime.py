#!/usr/bin/env python3


class Time:
    """HH:MM simplistic time"""

    def __init__(self, hrs=0, mins=0):
        self.hrs = hrs
        self.mins = mins

    def __repr__(self):
        return f"{self.hrs:02d}:{self.mins:02d}"

    def __add__(self, t):
        """Return Time + t, where t is another Time or Int mins"""
        tot = t + abs(self)
        return Time.from_int(tot)

    def __abs__(self):
        """Returns the total minutes represented"""
        return (self.hrs * 60) + self.mins

    @classmethod
    def from_int(cls, i):
        return cls(*divmod(i, 60))

    @classmethod
    def from_string(cls, string):
        if ':' in string:
            parts = [int(x) for x in string.split(':')]
            return cls(parts[0], parts[1])
        else:
            return cls.from_int(int(string))


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("Start", type=Time.from_string,
                        help="HH:MM or mins, representing beginning time")
    parser.add_argument("Elapse", type=Time.from_string,
                        help="HH:MM or mins, representing time to elapse by")
    parser.add_argument('-q', '--quiet', action='store_true',
                        help="Print elapsed time only")
    args = parser.parse_args()

    elapse = abs(args.Elapse)
    if args.quiet:
        print(f"{args.Start + elapse}")
    else:
        print(f"Start:\t{args.Start}\t{elapse:+}\nEnd:\t{args.Start + elapse}")
