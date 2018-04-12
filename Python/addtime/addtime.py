import sys

if len(sys.argv) == 1:
	Istr = raw_input("Start time:	")
	Istr = Istr.replace(' ', ':').split(':', 2)
else:
	Istr = sys.argv[1].split(':')
	if len(sys.argv) >= 3:
		Istr[len(Istr):] = [sys.argv[2]]

Istr = [int(x) for x in Istr]
if len(Istr) == 2: # Supplement input
	S_time = Istr
	elapsed = input("Period(min):	")

else: # Use existing
	S_time = Istr[:2] # Take 1st 2 elements
	elapsed = Istr[2]

def calcE_time(t, St = []):
	offset = St[0] * 60 + St[1]
	tot = t + offset
	return [tot // 60, tot % 60]

E_time = calcE_time(elapsed, S_time)
Ostr = "Start time:	{0}:{1}	+{2}min\nEnd time:	{3}:{4}".format(S_time[0], S_time[1], elapsed, E_time[0], E_time[1])

print(Ostr)
