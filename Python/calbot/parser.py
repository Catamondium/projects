#!/usr/bin/env python3
from datetime import date, datetime as dt
from time import strftime, localtime
from collections import namedtuple
from csv import reader
from itertools import takewhile
from functools import reduce, partial
from operator import add, ne

Holiday = namedtuple("Holiday", "start end")


def gtime(event):
    """Convert date() object into Google calendar api UTC-ISO timestamp
    """
    return dt(event.year, event.month, event.day).isoformat() + strftime("%z", localtime())


def tparse(string):
    """Convert "DD/MM/YYYY" into UTC-ISO date"""
    triplet = string.split("/")
    triplet = [int(x) for x in triplet]
    return gtime(date(triplet[2], triplet[1], triplet[0]))


def decomment(csvfile, symbol='#'):
    """Strip line comments, delimited by symbol"""
    for line in csvfile:
        nline = takewhile(partial(ne, symbol), line)
        stripped = reduce(add, nline).strip()
        if stripped:
            yield stripped


def parse(f):
    """Parse holiday descriptors."""
    data = []  # all structs
    for start, end in reader(decomment(f), delimiter=' ', skipinitialspace=True):
        data.append(Holiday(tparse(start), tparse(end)))
    return data


if __name__ == "__main__":
    from sys import argv
    with open(argv[1], 'r') as file:
        data = map("{}".format, parse(file))
        print("[\n%s\n]" % '\n'.join(data))
