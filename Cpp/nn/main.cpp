#include <iostream>
#include <fstream> //  PARSE     ifstream
#include <ctime> //    NOTE      tm time_t
#include <algorithm> //PARSE     transform, find_if
#include <cctype> //   PARSE     tolower              ??
#include <locale> //   PARSE     tolower, isspace     ??
#include <utility> //  PARSE     pair

/* TODO
 * extract parsing stuff into lib
 * extract Note into lib
 * implement argv IO
 * implement interactive IO
 * notification system
 */

class Note {
	std::string heading;
	std::tm *dueTime;
	std::string msg;

	public:
	Note(std::string heading, std::tm *dueTime, std::string msg):
		heading(heading), dueTime(dueTime), msg(msg) {}

	std::string getHeading() { return heading; }
	std::string getMsg() { return msg; }
	std::tm* getDue() { return dueTime; }
};

std::tm* tm_today() {
	std::time_t today_t = std::time(nullptr);
	std::tm *today = gmtime(&today_t);
	return today;
}
// PARSING funcs
enum Keyword { HEADING, DUE, EOE, MSG};

std::string trim(std::string s) {
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
				return !std::isspace(ch);
				})); // trim left

	s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
				return !std::isspace(ch);
				}).base(), s.end()); // trim right

	return s;
}

Keyword fEnum(std::string s) {
	s = trim(s);
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	
	if(s == "heading")
		return HEADING;

	else if(s == "due")
		return DUE;

	else {
		std::cerr << "ERROR: bad field name\n" << std::endl;
		std::exit(1);
	}
}

std::pair<Keyword, int> getkwd(std::string line) {
	std::size_t fPos;
	if(line.substr(0, 2) == "##")
		return std::pair<Keyword, int>(EOE, -1);
	else if ((fPos = line.find(":")) != std::string::npos)
		return std::pair<Keyword, int>(fEnum(line.substr(0, fPos)), fPos+1);
	
	return std::pair<Keyword, int>(MSG, -1);
}
///////
int main(int argc, char** argv) {
	// parse()
	std::string line;
	std::ifstream file("notes");
	size_t n = 0;
	while(std::getline(file, line)) {
		std::pair<Keyword, int> v = getkwd(line);
		switch(v.first) {
			case HEADING: {
				std::string value = trim(line.substr(v.second));
				std::cout << "Head:\t" << value << std::endl;
						  }
				break;
			case DUE: {
				std::string sDue = trim(line.substr(v.second));
				std::cout << "Due:\t" <<  sDue << std::endl;
					  }
				break;
			case EOE: {
				std::cout << "EOE" << std::endl;
					  }
				break;
			default: {
				std::cout << n << ":\t" << line << std::endl;
					 }
				break;
		}
		n++;
	}
	std::cout << "EOF:\t" << file.eof() << std::endl;
	////////
}
