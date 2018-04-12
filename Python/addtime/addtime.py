Istr = raw_input("Start time:	")
Istr = Istr.replace(' ', ':').split(':', 2)

Istr = [int(x) for x in Istr]
if len(Istr) == 2: # If e.g 3:30
	S_time = Istr
	elapsed = input("Period(min):	")

else: # If e.g 3:30 60
	S_time = [Istr[0], Istr[1]]
	elapsed = Istr[2]

def calcE_time(t, St = []):
	offset = St[0] * 60 + St[1]
	tot = t + offset
	return [tot // 60, tot % 60]

E_time = calcE_time(elapsed, S_time)
Ostr = "\nStart time:	{0}:{1}	+{2}min\nEnd time:	{3}:{4}".format(S_time[0], S_time[1], elapsed, E_time[0], E_time[1])

print(Ostr)

print(str(sys.argv))
# Command line output test:	./addtime.pyc 3:30 111
#	['./addtime.pyc', '3:30', '111']
