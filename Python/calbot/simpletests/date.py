import datetime as dt

def Gtime(event):
    """
    Convert date() object into Google calendar api UTC-ISO timestamp
    """
    return dt.datetime(event.year, event.month, event.day).isoformat() + 'Z'

with open("date", "r") as f: # "DD/MM/YYYY"
    date_triplet = f.read().split('/')
    date_triplet = [int(x) for x in date_triplet]
print(date_triplet)

# date(YYYY, MM, DD)
bday = dt.date(date_triplet[2], date_triplet[1], date_triplet[0])
print("Bday OBJ:\t%r" % bday)

# google cal form
print("Bday gISO:\t%s" % Gtime(bday))
now = dt.date.today()
print("Today gISO:\t%s" % Gtime(now))
