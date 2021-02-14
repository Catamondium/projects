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
ix, iy = ci.In(), ci.In()
ox = ci.Out()

xor = ci.Circuit.fromRAW(
    ox.input(
        ci.And(
            ci.Or(ix, iy),
            ci.Not(ci.And(ix, iy)))))
print(xor)
print(xor.debug())
print(xor.data)

try:
    for x in False,True:
        for y in False,True:
            out = xor.eval(ix= x, iy= y)
            print(f"{x},\t{y}\t= {out}")
            assert(out['ox'] == (x ^ y))
except ci.InputConflict as e:
    print(e)