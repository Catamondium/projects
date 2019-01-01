#pragma once
#include <optional>
#include <chrono> // timepoint

typedef std::chrono::system_clock::time_point note_time;
struct Note {
	std::string heading;
	std::optional<std::string> body;
	std::optional<note_time> event;

	Note(std::string heading, std::optional<std::string> body, std::optional<note_time> event):
		heading(heading), body(body), event(event) {}

	std::optional<std::string> printEvent();
	std::string unmarshal();
};
