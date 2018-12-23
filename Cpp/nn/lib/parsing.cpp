// types/containers
#include <vector>
#include <optional>
#include <utility>   // pair
#include <chrono>    // timepoint
#include "note.hpp"
// streams
#include <fstream>   // ifstream
#include <iostream>
#include <iomanip>   // get_time
#include <sstream>
// functional/locale
#include <algorithm> // transform, find_if
#include <cctype>    // tolower              ??
#include <locale>    // tolower, isspace     ??



namespace notelib {
	enum Keyword { HEADING, EVENT, EOE, BODY };

	std::string ltrim(std::string &s) {
		s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
					return !std::isspace(ch);
					}));
		return s;
	}

	std::string rtrim(std::string &s) {
		s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
					return !std::isspace(ch);
					}).base(), s.end());
		return s;
	}

	std::string trim(std::string &s) {
		ltrim(s);
		rtrim(s);
		return s;
	}

	Keyword fEnum(std::string s) {
		rtrim(s);
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

		else if ((fPos = line.find(':')) != std::string::npos)
			return std::pair<Keyword, int>(fEnum(line.substr(0, fPos)), fPos+1);
		
		return std::pair<Keyword, int>(BODY, -1);
	}
	
	note_time makeEvent(std::string value) {
		note_time ret;
		std::tm tm = {};
		std::stringstream ss(value);

		if(value.find('/') != std::string::npos) {
			ss >> std::get_time(&tm, "%d/%m/%Y");
			ret = std::chrono::system_clock::from_time_t(std::mktime(&tm));
		} else {
			std::cerr << "Malformed time" << std::endl;
			exit(1);
		}

		return ret;
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
			trim(line);
			switch(v.first) {
				case HEADING:
					line = line.substr(v.second);
					head = ltrim(line);
					break;
				case EVENT:
					line = line.substr(v.second);
					event = makeEvent(ltrim(line));
					break;
				case EOE:
					if(head)
						notes.push_back(Note(head.value(), body, event));
					head.reset();
					body.reset();
					break;
				default:
					line += "\n";
					body = (body) ? body.value() + line : line;
					break;
			}
		}

		if(head)
			notes.push_back(Note(head.value(), body, event));

		return notes;
	}

	void unmarshAll(std::vector<Note> notes, std::string fname) {
		std::ofstream file;
		file.open(fname, std::ofstream::trunc);

		for(Note note : notes) {
			file << note.unmarshal() << std::endl;
		}
	}
}
