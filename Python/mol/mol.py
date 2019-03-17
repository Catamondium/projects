#!/usr/bin/env python3
import re
import os
import sys
# beginning coefficient tokens groups: Elem, [coeff] blanking off subexprs
coeffRe = r"^(\d+)"
tokRe = r"\(.*?\)|([A-Z][a-z]*)([0-9]*)"
subRe = r"\((.*)\)([0-9]*)"  # subexpr tokens groups: expr, [coeff]
# Normalize nested exprs
trans = str.maketrans("{[()]}", "((()))")

ptable = {'': 0.00}


def loadTable(fname="ptable.dsv"):
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
    match = re.findall(tokRe, thing)
    subs = re.findall(subRe, thing)
    big = re.match(coeffRe, thing)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in match:
        if e not in ptable:
            raise Exception("Element %r doesn't exist" % e)
        if c == "":
            coeff = 1
        else:
            coeff = int(c)
        acc += ptable[e] * coeff

    for e, c in subs:
        if c == "":
            coeff = 1
        else:
            coeff = int(c)
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
