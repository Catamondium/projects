#!/usr/bin/env python3
import re
from pathlib import Path
from csv import reader
COEFFRE = r"^(\d+)"  # beginning coefficient
ELEM = r"([A-Z][a-z]*)"
TOKRE = r"\(.*?\)|" + ELEM + r"(\d*)"  # groups: Elem, [coeff]
SUBRE = r"\((.*)\)(\d*)"  # groups: expr, [coeff]
TRANS = str.maketrans("{}[]", "()()")


def make_coeff(c):
    if c == "":
        return 1
    else:
        return int(c)


def load_table(fname="ptable.tsv"):
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


load_table()


def valid(c):
    return c.isalnum() or c == "(" or c == ")"


def sanitize(comp):
    comp = comp.translate(TRANS)
    comp = [c for c in comp if valid(c)]
    return ''.join(comp)


class ElementError(Exception):
    def __init__(self, elem):
        super().__init__()
        self.elem = elem

    def __repr__(self):
        return f"Element \"{self.elem}\" doesn't exist"


def mass(comp):
    big = re.match(COEFFRE, comp)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in re.findall(TOKRE, comp):
        if e not in ptable:
            raise ElementError(e)
        coeff = make_coeff(c)
        acc += ptable[e] * coeff

    for e, c in re.findall(SUBRE, comp):
        coeff = make_coeff(c)
        acc += mass(e) * coeff

    return bigCoeff * acc


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
        description="Calculate molecular masses from structural formulae")
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
