# Python 3.5.2
import sys

class Time:
        def __init__(self, hrs, mins):
                self.hrs = hrs
                self.mins = mins
                
        def calcEnd(self, t):
                offset = self.hrs * 60 + self.mins
                tot = t + offset
                return Time(tot // 60, tot % 60)

if len(sys.argv) == 1:
	Istr = input("Start time:	")
	Istr = Istr.replace(' ', ':').split(':', 2)
else:
	Istr = sys.argv[1].split(':')
	if len(sys.argv) >= 3:
		Istr[len(Istr):] = [sys.argv[2]]

Istr = [int(x) for x in Istr]
if len(Istr) == 2:  # Supplement input
	elapse = eval(input("Period(min):	"))

else:  # Use existing
	elapse = Istr[2]
	
start = Time(Istr[0], Istr[1])
end = start.calcEnd(elapse)

Ostr_S = "Start time:	{0:02d}:{1:02d}".format(start.hrs, start.mins)
Ostr_E = "End time:	{0:02d}:{1:02d}".format(end.hrs, end.mins)

print("{0}	{1:+}min\n{2}".format(Ostr_S, elapse, Ostr_E))
