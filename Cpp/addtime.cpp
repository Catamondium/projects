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
		int abs() const;
		friend Time pTime(string in);
};

int Time::abs() const {
	// Get total minutes represented
	return (hrs * 60) + mins;
}

Time pTime(string in) {
	int delimpos = in.find(':');
	string str_hrs = in.substr(0, delimpos);
	string str_mins = in.substr(delimpos + 1, in.length());
	return Time(stoi(str_hrs), stoi(str_mins));
}

Time operator+(const Time lhs, const int rhs) {
	int tot = lhs.abs() + rhs;
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

	Time start = pTime(argv[1]);
	int elapse = (string(argv[2]).find(':') != string::npos) ?
		pTime(argv[2]).abs() : atoi(argv[2]);
	
	cout << "Start:\t" << start << "\t"
		<<  showpos << elapse << "\n"
		<< "End:\t" << start + elapse << endl;
	return 0;
}
