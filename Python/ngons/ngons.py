# Python 3.5.2

from turtle import *
import math

class ngonR:
    def __init__(self, n, R, x, y):
        self.n = n
        self.R = R
        self.y = y # True centre
        self.x = x # True centre

    def draw(self, col_L, col_F):
        length = self.R * 2 * math.sin(math.pi / self.n)
        Terry = Turtle()
        Terry.color(col_L, col_F)
        Terry.pu()
        Terry.setpos(.5 * length + self.x,
                - (self.R**2 - (.5 * length)**2)**.5 + self.y)
        Terry.begin_fill()
        Terry.pd()
        for i in range(0, self.n):
            Terry.left(180 - ((1 - (2 / self.n)) * 180))
            Terry.forward(length)
        Terry.end_fill()
        Terry.setpos(self.x, self.y)
        Terry.pu()

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
