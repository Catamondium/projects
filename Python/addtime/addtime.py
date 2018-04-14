# Python 3.5.2
import sys

if len(sys.argv) == 1:
	Istr = input("Start time:	")
	Istr = Istr.replace(' ', ':').split(':', 2)
else:
	Istr = sys.argv[1].split(':')
	if len(sys.argv) >= 3:
		Istr[len(Istr):] = [sys.argv[2]]

Istr = [int(x) for x in Istr]
if len(Istr) == 2:  # Supplement input
	Stime = Istr
	elapse = eval(input("Period(min):	"))

else:  # Use existing
	Stime = Istr[:2]  # Take 1st 2 elements
	elapse = Istr[2]


def calcEtime(St=[], t):
	offset = St[0] * 60 + St[1]
	tot = t + offset
	return [tot // 60, tot % 60]

Etime = calcEtime(Stime, elapse)
Ostr_S = "Start time:	{0:02d}:{1:02d}".format(Stime[0], Stime[1])
Ostr_E = "End time:	{0:02d}:{1:02d}".format(Etime[0], Etime[1])
Ostr = "{0}	{1:+}min\n{2}".format(Ostr_S, elapse, Ostr_E)

print(Ostr)
