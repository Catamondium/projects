#include <vector>
#include <optional>
#include <fstream>   // ifstream
#include <algorithm> // transform, find_if
#include <cctype>    // tolower              ??
#include <locale>    // tolower, isspace     ??
#include <utility>   // pair
#include <iostream>
#include "note.hpp"

namespace parsing {
	enum Keyword { HEADING,/* DUE,*/ EOE, MSG};

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

		//else if(s == "due")
		//	return DUE;

		else {
			std::cerr << "ERROR: bad field name\n" << std::endl;
			std::exit(1);
		}
	}

	std::pair<Keyword, int> getkwd(std::string line) {
		size_t fPos;
		if(line.substr(0, 2) == "##")
			return std::pair<Keyword, int>(EOE, -1);

		else if ((fPos = line.find(":")) != std::string::npos)
			return std::pair<Keyword, int>(fEnum(line.substr(0, fPos)), fPos+1);
		
		return std::pair<Keyword, int>(MSG, -1);
	}

	std::vector<Note::Note> parse(std::string fname) {
		std::vector<Note::Note> notes;

		std::string line;
		std::ifstream file(fname);

		std::optional<std::string> head;
		std::optional<std::string> msg;
		while(std::getline(file, line)) {
			std::pair<Keyword, int> v = getkwd(line);
			switch(v.first) {
				case HEADING:
					head = trim(line.substr(v.second));
					break;
				//case DUE: {
				//	std::string sDue = trim(line.substr(v.second));
				//	std::cout << "Due:\t" <<  sDue << std::endl;
				//		  }
				//	break;
				case EOE:
					if(head)
						notes.push_back(Note::Note(head.value(), msg));
					head.reset();
					msg.reset();
					break;
				default:
					line += "\n";
					if(msg)
						msg.value() += line;
					else
						msg = line;
			}
		}

		if(head)
			notes.push_back(Note::Note(head.value(), msg));

		return notes;
	}
}
