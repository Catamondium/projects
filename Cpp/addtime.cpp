#include <string>
#include <iostream> // showpos/noshowpos
#include <iomanip> // setw, setfill
#include <math.h> // floor

using namespace std;

class Time {
	public:
		int hrs;
		int mins;
		Time(int h, int m): hrs(h), mins(m) {}
};

Time operator+(const Time lhs, const int rhs) {
	int offset = lhs.hrs * 60 + lhs.mins;
	int tot = rhs + offset;
	return Time(floor(tot / 60), tot % 60);
}		

ostream& operator<<(ostream& stream, Time a) {
	char buf[6];
	sprintf(buf, "%02d:%02d", a.hrs, a.mins);
	stream << buf; // c-style format solution

	//stream << noshowpos << setfill('0') << setw(2) << a.hrs << ':' 
	//	<< setw(2) << a.mins; // c++ format solution
	return stream;
}

int main(int argc, char **argv) {
	if(argc < 3) {
		cout << "Error:\thh:mm mins expected." << endl;
		return 1;
	}
	string strT = argv[1];
	int delimpos = strT.find(':');
	string str_hrs = strT.substr(0, delimpos);
	string str_mins = strT.substr(delimpos + 1, strT.length());
	Time start(stoi(str_hrs), stoi(str_mins));
	int elapse = atoi(argv[2]);
	
	cout << "Start:\t" << start << "\t"
		<<  showpos << elapse << "\n"
		<< "End:\t" << start + elapse << endl;
	return 0;
}
