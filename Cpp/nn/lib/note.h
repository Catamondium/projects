#pragma once
#include <optional>
#include <chrono> // timepoint

using note_time = std::chrono::system_clock::time_point;
struct Note
{
	std::string heading;
	std::string body;
	std::optional<note_time> event;

	Note(std::string heading = "", std::string body = "", std::optional<note_time> event = {}) : heading(heading), body(body), event(event) {}

	std::optional<std::string> printEvent();
	std::string unmarshal();
};

bool operator==(const Note &lhs, const Note &rhs) noexcept;
