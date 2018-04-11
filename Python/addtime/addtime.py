instr = raw_input("Start time:	").split(':', 1) # Collect [hrs, min] as string
elapsed = input("Period(min):	")
S_time = [int(x) for x in instr] # Cast to int

# Undecided about including functions.
#def get_min(t): # Get multiples of minutes
	#return t % 60

#def get_hrs(t): # Get multiples of hours
	#return t // 60

#def hrs_min(t): # Hours to minutes
	#return t * 60

def calcE_time(t, St = []):
	offset = St[0] * 60 + St[1]
	tot = t + offset
	return [tot // 60, tot % 60]

E_time = calcE_time(elapsed, S_time)
outstr = "\nStart time:	{0}:{1}	+{2}min\nEnd time:	{3}:{4}".format(S_time[0], S_time[1], elapsed, E_time[0], E_time[1])

print(outstr)
