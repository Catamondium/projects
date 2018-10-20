import datetime as dt
from collections import namedtuple

Holiday = namedtuple("Holiday", ["start", "end"]) # primary struct
data = [] # all structs

def sig(func):
    """Print the decorated function's running signature.
    """
    name = func.__name__
    def wrap(*args):
        argstr = ", ".join(str(arg) for arg in args)
        result = func(*args)
        print("%s(%s):\t%s" % (name, argstr, result))
        return result
    return wrap

def printDates():
    strings = []
    for start, end in data:
        strings.append("(%s)" % (", ".join((str(start), str(end)))))
    print(",\n".join(strings))

def Gtime(event):
    """Convert date() object into Google calendar api UTC-ISO timestamp
    """
    return dt.datetime(event.year, event.month, event.day).isoformat() + 'Z'

def tparse(string):
    """Convert "DD/MM/YYYY" into date() object."""
    triplet = string.split("/")
    triplet = [int(x) for x in triplet]
    return dt.date(triplet[2], triplet[1], triplet[0])

# main()
with open("data", "r") as f: # Parse ranges
    for pair in f.read().split("\n"):
        dates = pair.split(' ')
        if dates[0] == '':
            break
        data.append(
                Holiday(tparse(dates[0]), tparse(dates[1])))
printDates()
