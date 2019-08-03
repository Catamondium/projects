#pragma once
#include <optional>
#include "note.hpp"
using systime = std::chrono::system_clock;

namespace util
{
std::string ltrim(std::string &s);
std::string rtrim(std::string &s);
std::string trim(std::string &s);
std::optional<note_time> makeEvent(std::string value);
} // namespace util