S_hrs = input("Start time hours:	")
S_min = input("Start time minutes:	")
elapsed = input("Movie length(min):	")
S_time = [S_hrs, S_min]

def get_min(t): # Get multiples of minutes
	return t % 60

def get_hrs(t): # Get multiples of hours
	return t // 60

def hrs_min(t): # Hours to minutes
	return t * 60

def calcE_time(t, St = []):
	offset = hrs_min(St[0]) + St[1]
	tot = t + offset
	return [get_hrs(tot), get_min(tot)]

E_time = calcE_time(elapsed, S_time)
outstr = "\nStart time:	{0}:{1}	+{2}min\nEnd time:	{3}:{4}".format(S_time[0], S_time[1], elapsed, E_time[0], E_time[1])

print(outstr)
