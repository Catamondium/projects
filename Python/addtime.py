# Python 3.5.2
import sys

class Time:
        def __init__(self, hrs, mins):
                self.hrs = hrs
                self.mins = mins
                
        def __repr__(self):
                return "{0:02d}:{1:02d}".format(self.hrs, self.mins)
                
        def __add__(self, t):
                tot = t + abs(self);
                return Time(tot // 60, tot % 60)

        def __abs__(self):
                return (self.hrs * 60) + self.mins

def pTime(string):
    Istr = sys.argv[1].split(':')
    parts = [int(x) for x in Istr]
    return Time(parts[0], parts[1])


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Error:\thh:mm mins expected")
        sys.exit(1)

    start = pTime(sys.argv[1])

    if ":" in sys.argv[2]:
        elapse = abs(pTime(sys.argv[2]))
    else:
        elapse = int(sys.argv[2])

    Ostr_S = "Start:\t{0}".format(start)
    Ostr_E = "End:\t{0}".format(start + elapse)

    print("{0}\t{1:+}\n{2}".format(Ostr_S, elapse, Ostr_E))
