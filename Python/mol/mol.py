#!/usr/bin/env python3
import re
import os
import sys
COEFFRE = r"^(\d+)"  # beginning coefficient
TOKRE = r"\(.*?\)|([A-Z][a-z]*)(\d*)"  # groups: Elem, [coeff]
SUBRE = r"\((.*)\)(\d*)"  # groups: expr, [coeff]
TRANS = str.maketrans("{}[]", "()()")

ptable = {'': 0.00}


def makeCoeff(c):
    if c == "":
        return 1
    else:
        return int(c)


def loadTable(fname="ptable.tsv"):
    script = os.path.realpath(__file__)
    script_dir = os.path.dirname(script)
    tpath = os.path.join(script_dir, fname)
    with open(tpath, 'r') as f:
        next(f)  # skip header
        for i, line in enumerate(f):
            k, v = line.strip().split('\t')
            if k in ptable:
                raise(f"Table error: line {i}, Repeated element {k}")
            else:
                ptable[k] = float(v)


def valid(c):
    return c.isalnum() or c == "(" or c == ")"


def sanitize(thing):
    thing = thing.translate(TRANS)
    thing = [c for c in thing if valid(c)]
    return ''.join(thing)


def mass(thing):
    big = re.match(COEFFRE, thing)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in re.findall(TOKRE, thing):
        if e not in ptable:
            raise Exception(f"Element \"{e}\" doesn't exist")
        coeff = makeCoeff(c)
        acc += ptable[e] * coeff

    for e, c in re.findall(SUBRE, thing):
        coeff = makeCoeff(c)
        acc += mass(e) * coeff

    return bigCoeff * acc


loadTable()

if __name__ == "__main__":
    for comp in sys.argv[1:]:
        clean = sanitize(comp)
        try:
            print(f"{clean}:\t{mass(clean):.2f} g/mol")
        except Exception as e:
            print(e)
            exit(1)
