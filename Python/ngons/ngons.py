# Python 3.5.2

from turtle import *
import sys
import math

class ngonR:
    def __init__(self, n, R, x, y):
        self.n = n
        self.R = R
        self.y = y # True centre
        self.x = x # True centre

    def update(self):
        self.len = self.R * 2 * math.sin(math.pi / self.n)
        self.oy = - (self.R**2 - (.5 * self.len)**2)**.5 + self.y # Begin vertex
        self.ox = .5 * self.len + self.x # Begin vertex

    def draw(self, col_L, col_F):
        self.update()
        t = Turtle()
        t.color(col_L, col_F)
        t.pu()
        t.setpos(self.ox, self.oy)
        t.begin_fill()
        t.pd()
        for i in range(0, self.n):
            t.left(180 - ((1 - (2 / self.n)) * 180))
            t.forward(self.len)
        t.end_fill()
        t.setpos(self.x, self.y)
        t.pu()

# Main
bgcolor("black")
shapes = []
for n in range(3, 10):
        shape = ngonR(n, 200, 0, 0)
        shape.draw("red", "purple")
# Screenshot
canvas = getcanvas()
canvas.postscript(file='out.ps')
exitonclick()
