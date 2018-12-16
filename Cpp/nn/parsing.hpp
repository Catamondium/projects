#pragma once
#include <vector>
#include "note.hpp"

namespace parsing {
	std::string trim(std::string);
	std::vector<Note::Note> parse(std::string);
}
