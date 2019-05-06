from datetime import date, datetime
from time import strftime, localtime
from collections import namedtuple
from csv import reader

Holiday = namedtuple("Holiday", "start end")


def Gtime(event):
    """Convert date() object into Google calendar api UTC-ISO timestamp
    """
    return datetime(event.year, event.month, event.day).isoformat() + strftime("%z", localtime())


def tparse(string):
    """Convert "DD/MM/YYYY" into UTC-ISO date"""
    triplet = string.split("/")
    triplet = [int(x) for x in triplet]
    return Gtime(date(triplet[2], triplet[1], triplet[0]))


def parse(f):
    """Parse holiday descriptors."""
    data = []  # all structs
    with open(f, "r") as f:  # Parse ranges
        for (start, end) in reader(f, delimiter='\t'):
            data.append(Holiday(tparse(start), tparse(end)))
    return data


if __name__ == "__main__":
    from sys import argv
    file = argv[1]
    data = map("{}".format, parse(file))
    print("[\n%s\n]" % '\n'.join(data))
