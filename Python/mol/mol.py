#!/usr/bin/env python3
import re
from pathlib import Path
from csv import reader
COEFFRE = r"^(\d+)"  # beginning coefficient
ELEM = r"([A-Z][a-z]*)"
TOKRE = r"\(.*?\)|" + ELEM + r"(\d*)"  # groups: Elem, [coeff]
SUBRE = r"\((.*)\)(\d*)"  # groups: expr, [coeff]
TRANS = str.maketrans("{}[]", "()()")


def makeCoeff(c):
    if c == "":
        return 1
    else:
        return int(c)


def loadTable(fname="ptable.tsv"):
    tpath = Path(__file__).resolve().parent / fname
    global ptable
    ptable = {'': 0.00}
    with open(tpath, 'r') as f:
        next(f)  # skip header
        for i, (k, v) in enumerate(reader(f, delimiter='\t')):
            if k in ptable:
                raise Exception(f"Table error: line {i}, Repeated element {k}")
            else:
                ptable[k] = float(v)


def valid(c):
    return c.isalnum() or c == "(" or c == ")"


def sanitize(thing):
    thing = thing.translate(TRANS)
    thing = [c for c in thing if valid(c)]
    return ''.join(thing)


class ElementError(BaseException):
    def __init__(self, elem):
        super().__init__()
        self.elem = elem

    def __repr__(self):
        return f"Element \"{self.elem}\" doesn't exist"


def mass(thing):
    big = re.match(COEFFRE, thing)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in re.findall(TOKRE, thing):
        if e not in ptable:
            raise ElementError(e)
        coeff = makeCoeff(c)
        acc += ptable[e] * coeff

    for e, c in re.findall(SUBRE, thing):
        coeff = makeCoeff(c)
        acc += mass(e) * coeff

    return bigCoeff * acc


loadTable()

if __name__ == "__main__":
    import argparse
    import sys
    parser = argparse.ArgumentParser(
        description="Calculate molecular mass from structural formulae")
    parser.add_argument("comps", metavar="Compound", nargs='+',
                        help="Structural formula to be evaluated")
    args = parser.parse_args()

    for comp in args.comps:
        clean = sanitize(comp)
        try:
            print(f"{clean}:\t{mass(clean):.2f} g/mol")
        except ElementError as e:
            print("%r" % e)
            exit(1)
