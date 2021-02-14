#!/bin/envrun
import z3
import circ as ci

print("z3------")
# XOR test case
# (A + B)* ~(AB)
x = z3.Bool('x')
y = z3.Bool('y')

expr = z3.And( # 'z'
    z3.Or(x, y),
    z3.Not(z3.And(x, y))
)

print(expr)

print("internal-------")
cx, cy = ci.In(), ci.In()
cz = ci.Out()
symbols = [
    cx, cy,

    ci.Or().input(cx, cy),
    ci.Not(),
    ci.And().input(cx, cy), ci.And(),

    cz
]

symbols[3].input(symbols[4])
symbols[5].input(symbols[2], symbols[3])
cz.input(symbols[5])

#ca.setName('a')
#cb.setName('b')
#cz.setName('z')

"""
# IDs reflect notes
for i,sym in enumerate(symbols):
    sym._id = i
for sym in symbols:
    print(repr(sym))
"""

xor = ci.Circuit.fromRAW(*symbols)
print(xor)
print(xor.debug())

for x in False,True:
    for y in False,True:
        out = xor.eval(x= x, y= y)
        print(f"{x},\t{y}\t= {out}")
        assert(out['z'] == (x ^ y))