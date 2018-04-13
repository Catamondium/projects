# Python 2.7.12

import sys

if len(sys.argv) == 1:
        Istr = raw_input("Start time:	")
        Istr = Istr.replace(' ', ':').split(':', 2)
else:
        Istr = sys.argv[1].split(':')
        if len(sys.argv) >= 3:
            Istr[len(Istr):] = [sys.argv[2]]

Istr = [int(x) for x in Istr]
if len(Istr) == 2:  # Supplement input
    Stime = Istr
    elapse = input("Period(min):	")

else:  # Use existing
        Stime = Istr[:2]  # Take 1st 2 elements
        elapse = Istr[2]


def calcEtime(t, St=[]):
        offset = St[0] * 60 + St[1]
        tot = t + offset
        return [tot // 60, tot % 60]

Etime = calcEtime(elapse, Stime)
Ostr_1 = "Start time:	{0:02d}:{1:02d}	+{2}min".format(Stime[0], Stime[1], elapse)
Ostr_2 = "\nEnd time:	{0:02d}:{1:02d}".format(Etime[0], Etime[1])

print((Ostr_1 + Ostr_2))
