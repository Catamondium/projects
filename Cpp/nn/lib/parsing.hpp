#pragma once
#include <vector>
#include "note.hpp"

namespace notelib {
	std::string trim(std::string);
	std::vector<Note> parse(std::string);
	void unmarshAll(std::vector<Note>, std::string);
}
