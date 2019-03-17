#!/usr/bin/env python3
import re
import os
import sys
# beginning coefficient tokens groups: Elem, [coeff] blanking off subexprs
coeffRe = r"^(\d+)"
tokRe = r"\(.*?\)|([A-Z][a-z]*)(\d*)"
subRe = r"\((.*)\)(\d*)"  # subexpr tokens groups: expr, [coeff]
# Normalize nested exprs
trans = str.maketrans("{[()]}", "((()))")

ptable = {'': 0.00}


def makeCoeff(c):
    if c == "":
        return 1
    else:
        return int(c)


def loadTable(fname="ptable.csv"):
    script = os.path.realpath(__file__)
    script_dir = os.path.dirname(script)
    tpath = os.path.join(script_dir, fname)
    with open(tpath, 'r') as f:
        for line in f:
            k, v = line.strip().split('|')
            ptable[k] = float(v)


def valid(c):
    return c.isalnum() or c == "(" or c == ")"


def sanitize(thing):
    thing = thing.translate(trans)
    thing = [c for c in thing if valid(c)]
    return ''.join(thing)


def Mass(thing):
    big = re.match(coeffRe, thing)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in re.findall(tokRe, thing):
        if e not in ptable:
            raise Exception("Element %r doesn't exist" % e)
        coeff = makeCoeff(c)
        acc += ptable[e] * coeff

    for e, c in re.findall(subRe, thing):
        coeff = makeCoeff(c)
        acc += Mass(e) * coeff

    return bigCoeff * acc


loadTable()

if __name__ == "__main__":
    for comp in sys.argv[1:]:
        clean = sanitize(comp)
        try:
            print("%s:\t%.2f" % (comp, Mass(comp)))
        except Exception as e:
            print(e)
            exit(1)
