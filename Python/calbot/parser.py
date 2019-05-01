from datetime import date, datetime
from collections import namedtuple
from csv import reader

Holiday = namedtuple("Holiday", ["start", "end"])


def printData(data):
    """Clean repr of Holiday tuple."""
    strings = []
    for start, end in data:
        strings.append("(%s)" % ", ".join((str(start), str(end))))
    print("[%s]" % ",\n".join(strings))


def Gtime(event):
    """Convert date() object into Google calendar api UTC-ISO timestamp
    """
    return datetime(event.year, event.month, event.day).isoformat() + 'Z'


def tparse(string):
    """Convert "DD/MM/YYYY" into date() object."""
    triplet = string.split("/")
    triplet = [int(x) for x in triplet]
    return date(triplet[2], triplet[1], triplet[0])


def parse(f):
    """Parse holiday descriptors."""
    data = []  # all structs
    with open(f, "r") as f:  # Parse ranges
        for (start, end) in reader(f, delimiter='\t'):
            if start == '':
                break
            data.append(Holiday(tparse(start), tparse(end)))
    data = [Holiday(Gtime(s), Gtime(e)) for s, e in data]
    return data


if __name__ == "__main__":
    print("Google times")
   # printData(dates)
