#include <string>
#include <iostream> // showpos/noshowpos
#include <iomanip> // setw, setfill
#include <math.h> // floor

class Time {
	public:
		int hrs;
		int mins;
		Time(int h, int m): hrs(h), mins(m) {}
		int abs() const;
		friend Time pTime(std::string in);
};

int Time::abs() const {
	// Get total minutes represented
	return (hrs * 60) + mins;
}

Time pTime(std::string in) {
	int delimpos = in.find(':');
	std::string str_hrs = in.substr(0, delimpos);
	std::string str_mins = in.substr(delimpos + 1, in.length());
	return Time(stoi(str_hrs), stoi(str_mins));
}

Time operator+(const Time lhs, const int rhs) {
	int tot = lhs.abs() + rhs;
	return Time(floor(tot / 60), tot % 60);
}		

std::ostream& operator<<(std::ostream& stream, Time a) {
	char buf[1000];
	sprintf(buf, "%02d:%02d", a.hrs, a.mins);
	stream << buf; // c-style format solution

	//stream << std::noshowpos << std::setfill('0') << std::setw(2) << a.hrs << ':' 
	//		<< std::setw(2) << a.mins; // c++ format solution
	return stream;
}

int main(int argc, char **argv) {
	if(argc < 3) {
			std::cout << "Error:\thh:mm mins expected." << std::endl;
		return 1;
	}

	Time start = pTime(argv[1]);
	int elapse = (std::string(argv[2]).find(':') != std::string::npos) ?
		pTime(argv[2]).abs() : atoi(argv[2]);
	
	std::cout << "Start:\t" << start << "\t"
		<<  std::showpos << elapse << "\n"
		<< "End:\t" << start + elapse << std::endl;
	return 0;
}
