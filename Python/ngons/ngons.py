from turtle import *
import sys
import math

def ty(n, R): # y component to beginning vertex of ngon-R
	length = getlength(n, R)
	return - (R**2 - (.5 * length)**2)**.5
def tx(n, R): # x component to beginning vertex of ngon-R
	length = getlength(n, R)
	return .5 * length

def getlength(n, R): # Find length of regular n-gon-R
	return R * 2 * math.sin(math.pi / n)

def ngon(n, R, t): # Generate regular n-gon-R, with turtle t
	side_length = getlength(n, R)
	for i in range(0, n):
		t.left(180 - ((1 - (2 / n)) * 180)) # t rotates by exterior angles
		t.forward(side_length)
		title('ngon: {}'.format(n))
# Settings
terry = Turtle()
bgcolor('black')
terry.color('red', 'purple')
terry.shape('turtle')
title(sys.argv[0])
# Draw shapes
r = 200
last_n = 0
for n in range(3, 8):
	terry.pu()
	terry.setpos(tx(n, r), ty(n, r))
	terry.pd()
	terry.begin_fill()
	ngon(n, r, terry)
	terry.end_fill()
	last_n = n
# Draw measured circumradius
str = 'R = {}'.format(round(terry.distance(0, 0)))
terry.color('black')
terry.setpos(tx(last_n, r) / 2, ty(last_n, r) / 2)
terry.write(str)
terry.home()
terry.color('red')
terry.dot()
#terry.ht()
# Screenshot
canvas = getcanvas()
canvas.postscript(file='out.ps')
exitonclick()
