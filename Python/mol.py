#!/usr/bin/env python3
import re
# beginning coefficient tokens groups: Elem, [coeff] blanking off subexprs
coeffRe = r"^(\d+)"
tokRe = r"\(.*?\)|([A-Z][a-z]*)([0-9]*)"
subRe = r"\((.*)\)([0-9]*)"  # subexpr tokens groups: expr, [coeff]
# Normalize nested exprs
trans = str.maketrans("{[()]}", "((()))")


def valid(c):
    return c.isalnum() or c == "(" or c == ")"


def sanitize(thing):
    thing = thing.translate(trans)
    clean = [c for c in thing if valid(c)]
    return ''.join(clean)


ptable = {
    '':  0,
    'C': 12,
    'H': 1,
    'O': 16
}


def Mass(thing):
    clean = sanitize(thing)
    match = re.findall(tokRe, clean)
    subs = re.findall(subRe, clean)
    big = re.match(coeffRe, clean)
    if big:
        bigCoeff = int(big[0])
    else:
        bigCoeff = 1

    acc = 0
    for e, c in match:
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


# simple = "C2H3OH"  # Ethanol mr=44
nested = "CH3C(CH3)2CH3"  # Dimethylpropane, C5H12 72
invalid = "Ooo"
#print("Mr:\t%d g/mol" % Mass(simple))
print("Mr:\t%d g/mol" % Mass(nested))  # Doubly counted nesting!!!
