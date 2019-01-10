#pragma once
#include <vector>
#include "note.hpp"

namespace notelib {
	std::string ltrim(std::string&);
	std::string rtrim(std::string&);
	std::string trim(std::string&);
	std::vector<Note> parse(std::string);
	std::optional<note_time> makeEvent(std::string value);
	void unmarshAll(std::vector<Note>&, std::string);
}
