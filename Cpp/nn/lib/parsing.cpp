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
			return {Keyword::EOE, -1};

		else if ((fPos = line.find(':')) != std::string::npos)
			return {fEnum(line.substr(0, fPos)), fPos+1};
		
		return {Keyword::BODY, -1};
	}

	note_time makeEvent(std::string value)
	{
		std::stringstream ss(value);
		std::tm tmp = {};

		if(value.find('/') != std::string::npos) {
			ss >> std::get_time(&tmp, "%d/%m/%Y %R");
		} else {
			std::cerr << "Malformed time" << std::endl;
			exit(1);
		}

		return systime::from_time_t(std::mktime(&tmp));
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
			auto [field, pos] = getkwd(line);
			trim(line);
			switch(field) {
				case Keyword::HEADING:
					line = line.substr(pos);
					head = ltrim(line);
					break;
				case Keyword::EVENT:
					line = line.substr(pos);
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
