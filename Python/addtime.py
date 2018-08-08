# Python 3.5.2
import sys

class Time:
        def __init__(self, hrs, mins):
                self.hrs = hrs
                self.mins = mins
                
        def __repr__(self):
                return "{0:02d}:{1:02d}".format(self.hrs, self.mins)
                
        def __add__(self, t):
                offset = self.hrs * 60 + self.mins
                tot = t + offset
                return Time(tot // 60, tot % 60)

if len(sys.argv) < 3:
    print("Error:\thh:mm mins expected")
    sys.exit(1)


Istr = sys.argv[1].split(':')
Istr[len(Istr):] = [sys.argv[2]]

Istr = [int(x) for x in Istr]

start = Time(Istr[0], Istr[1])
elapse = Istr[2]

Ostr_S = "Start:	{0}".format(start)
Ostr_E = "End:	{0}".format(start + elapse)

print("{0}	{1:+}min\n{2}".format(Ostr_S, elapse, Ostr_E))
