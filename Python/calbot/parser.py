#!/usr/bin/env python3
from datetime import datetime as dt
from time import strftime, localtime
from collections import namedtuple
from csv import reader
from itertools import takewhile, starmap
from functools import reduce, partial
from operator import add, ne

Holiday = namedtuple("Holiday", "start end")


def goog_conv(event):
    """
    Convert Datetime into Google UTC-ISO format
    """
    return dt(event.year, event.month, event.day).isoformat() + strftime("%z", localtime())


def tclsmap(cls, f, iterable):
    """
    'tuple class map'
    starmaps accross iterable and starmaps cls
    see source, not simple to describe
    """
    for t in iterable:
        tpl = ()
        for elem in t:
            tpl += (f(elem),)
        yield cls(*tpl)

GB_FORMAT = "%d/%m/%Y"

def gb_date(string):
    """Convert "DD/MM/YYYY" into Datetime"""
    return dt.strptime(string, GB_FORMAT)


def decomment(csvfile, symbol='#'):
    """Strip line comments, delimited by symbol"""
    for line in csvfile:
        nline = takewhile(partial(ne, symbol), line)
        stripped = reduce(add, nline).strip()
        if stripped:
            yield stripped


def parse(f):
    """Parse holiday descriptors."""
    ret = []
    for start, end in reader(decomment(f), delimiter=' ', skipinitialspace=True):
        ret.append(Holiday(gb_date(start), gb_date(end)))
    return ret


def fullparse(f):
    yield from tclsmap(Holiday, goog_conv, parse(f))


if __name__ == "__main__":
    from sys import argv
    with open(argv[1], 'r') as file:
        data = map("{}".format, fullparse(file))
        print("[\n%s\n]" % '\n'.join(data))
