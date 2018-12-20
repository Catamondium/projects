#include <vector>
#include <map>
#include <optional>
#include <fstream>   // ifstream
#include <algorithm> // transform, find_if
#include <cctype>    // tolower              ??
#include <locale>    // tolower, isspace     ??
#include <utility>   // pair
#include <chrono>    // timepoint
#include <iostream>
#include <iomanip>
#include <sstream>
#include "note.hpp"

namespace parsing {
	enum Keyword { HEADING, EVENT, EOE, BODY};

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

		else if(s == "event")
			return EVENT;

		else
			return BODY;
	}

	std::pair<Keyword, int> getkwd(std::string line) {
		size_t fPos;
		if(line.substr(0, 2) == "##")
			return std::pair<Keyword, int>(EOE, -1);

		else if ((fPos = line.find(":")) != std::string::npos)
			return std::pair<Keyword, int>(fEnum(line.substr(0, fPos)), fPos+1);
		
		return std::pair<Keyword, int>(BODY, -1);
	}
	
	note_duration dateResolve(std::string source) {
		std::chrono::system_clock::time_point ret;
		std::tm tm = {};
		std::stringstream ss(source);

		size_t fPos;
		if((fPos = source.find('/')) != std::string::npos) {
			ss >> std::get_time(&tm, "%d/%m/%Y");
			ret = std::chrono::system_clock::from_time_t(std::mktime(&tm));
		}
		else if ((fPos = source.find(':')) != std::string::npos) {
			ss >> std::get_time(&tm, "%H:%M");
			ret = std::chrono::system_clock::from_time_t(std::mktime(&tm));
				}
		else {
			std::cerr << "Malformed time" << std::endl;
			exit(1);
		}

		return std::chrono::duration_cast<note_duration>(ret.time_since_epoch());
	}

	note_time makeEvent(std::string value) {
		note_time baseDate; // epoch default construction
	
		std::stringstream ss;
		ss.str(value);
		std::string thing;
		while(ss >> thing) {
			baseDate += dateResolve(thing);
		}
		
		return baseDate;
	}

	std::vector<Note> parse(std::string fname) {
		std::vector<Note> notes;

		std::string line;
		std::ifstream file(fname);

		std::optional<std::string> head;
		std::optional<std::string> body;
		std::optional<note_time> event;
		while(std::getline(file, line)) {
			std::pair<Keyword, int> v = getkwd(line);
			switch(v.first) {
				case HEADING:
					head = trim(line.substr(v.second));
					break;
				case EVENT:
					event = makeEvent(trim(line.substr(v.second)));
					break;
				case EOE:
					if(head)
						notes.push_back(Note(head.value(), body, event));
					head.reset();
					body.reset();
					break;
				default:
					line += "\n";
					if(body)
						body = body.value() + line;
					else
						body = line;
					break;
			}
		}

		if(head)
			notes.push_back(Note(head.value(), body, event));

		return notes;
	}
}