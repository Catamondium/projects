// types/containers
#include <vector>
#include <optional>
#include <utility>   // pair
#include <chrono>    // timepoint
#include <ctime>     // time_t, tm, localtime
#include "note.hpp"
// streams
#include <iostream>
#include <iomanip>   // get_time
#include <fstream>
#include <sstream>
// functional/locale
#include <algorithm> // transform, find_if
#include <locale>    // tolower, isspace

using systime = std::chrono::system_clock;
std::string printEvent(note_time source) // debugging func
{
	std::time_t tt = systime::to_time_t(source);

	struct std::tm *tm = std::gmtime(&tt);
	std::stringstream ss;
	ss << std::put_time(tm, "%d/%m/%Y %R");
	return ss.str();
}

namespace notelib {
	enum struct Keyword { HEADING, EVENT, EOE, BODY };

	std::string ltrim(std::string &s)
	{
		s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
					return !std::isspace(ch);
					}));
		return s;
	}

	std::string rtrim(std::string &s)
	{
		s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
					return !std::isspace(ch);
					}).base(), s.end());
		return s;
	}

	std::string trim(std::string &s)
	{
		ltrim(s);
		rtrim(s);
		return s;
	}

	Keyword fEnum(std::string s)
	{
		rtrim(s);
		std::transform(s.begin(), s.end(), s.begin(), ::tolower);
		
		if(s == "heading")
			return Keyword::HEADING;

		else if(s == "event")
			return Keyword::EVENT;

		else
			return Keyword::BODY;
	}

	std::pair<Keyword, int> getkwd(std::string line)
	{
		size_t fPos;
		if(line.substr(0, 2) == "##")
			return std::make_pair(Keyword::EOE, -1);

		else if ((fPos = line.find(':')) != std::string::npos)
			return std::make_pair(fEnum(line.substr(0, fPos)), fPos+1);
		
		return std::make_pair(Keyword::BODY, -1);
	}

	note_time parseSeg(std::string str, note_time base)
	{
		// Base plan: break into calendar time,
		// substitute as appropriate
		// Reform into absolute time

		std::stringstream ss(str);

		std::time_t baseptr = systime::to_time_t(base);
		std::tm *civil_base = std::gmtime(&baseptr);
		// Ready for substitution
		
		if(str.find('/') != std::string::npos) {
			std::tm tmp = {};
			ss >> std::get_time(&tmp, "%d/%m/%Y");
			civil_base->tm_mday = tmp.tm_mday;
			civil_base->tm_mon = tmp.tm_mon;
			civil_base->tm_year = tmp.tm_year;
		} else if(str.find(':' != std::string::npos)) {
			// Getting through, but waaay off, could be a normalisation thing
			std::tm tmp = {};
			ss >> std::get_time(&tmp, "%R");
			civil_base->tm_hour = tmp.tm_hour;
			civil_base->tm_min = tmp.tm_min;
		} else {
			std::cerr << "Malformed time" << std::endl;
			exit(1);
		}

		// Ready for reconstitution
		std::time_t retptr = std::mktime(civil_base);
		note_time ret_absT = systime::from_time_t(retptr);

		std::cout << "parseSeg: " << printEvent(ret_absT) << std::endl;
		return ret_absT;
	}
	
	note_time makeEvent(std::string value)
	{
		note_time ret;
		std::tm tm = {};
		std::stringstream ss(value);

		/*std::stringstream test(value);
		std::string passin;
		while(test >> passin)
			parseSeg(passin, ret);*/

		if(value.find('/') != std::string::npos) {
			ss >> std::get_time(&tm, "%d/%m/%Y");
			ret = systime::from_time_t(std::mktime(&tm));
		} else {
			std::cerr << "Malformed time" << std::endl;
			exit(1);
		}

		return ret;
	}

	std::vector<Note> parse(std::string fname)
	{
		std::vector<Note> notes;

		std::string line;
		std::ifstream file(fname);

		std::optional<std::string> head;
		std::optional<std::string> body;
		std::optional<note_time> event;
		while(std::getline(file, line)) {
			std::pair<Keyword, int> v = getkwd(line);
			trim(line);
			switch(v.first) {
				case Keyword::HEADING:
					line = line.substr(v.second);
					head = ltrim(line);
					break;
				case Keyword::EVENT:
					line = line.substr(v.second);
					event = makeEvent(ltrim(line));
					break;
				case Keyword::EOE:
					if(head) {
						if(body)
							notes.push_back(Note(head.value(), rtrim(body.value()), event));
						else
							notes.push_back(Note(head.value(), body, event));
					}
					head.reset();
					body.reset();
					event.reset();
					break;
				default:
					line += "\n";
					body = (body) ? body.value() + line : line;
					break;
			}
		}

		if(head) {
			if(body)
				notes.push_back(Note(head.value(), rtrim(body.value()), event));
			else
				notes.push_back(Note(head.value(), body, event));
		}

		return notes;
	}

	void unmarshAll(std::vector<Note> notes, std::string fname)
	{
		std::ofstream file;
		file.open(fname, std::ofstream::trunc);

		for(Note note : notes) {
			file << note.unmarshal() << std::endl;
		}
	}
}
