#!/bin/envrun
import z3
import circ as ci

# XOR test case
# (A + B)* ~(AB)
a = z3.Bool('a')
b = z3.Bool('b')

expr = z3.And( # 'z'
    z3.Or(a, b),
    z3.Not(z3.And(a, b))
)
print("z3\n-------")
print(expr)

print("internal\n-------")
ca, cb = ci.In(), ci.In()
cz = ci.Out()
symbols = [
    ca, cb,

    ci.Or().input(ca, cb),
    ci.Not(),
    ci.And().input(ca, cb), ci.And(),

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

circuit = ci.Circuit.fromRAW(*symbols)
print(circuit.debug())