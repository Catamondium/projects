#pragma once
#include <optional>
#include <chrono> // time_point

typedef std::chrono::system_clock::time_point time_point;
class Note {
	std::string heading;
	std::optional<std::string> body;
	//std::optional<time_point> event;

	public:
	Note(
			std::string heading,
			std::optional<std::string> body/*,
			std::optional<time_point> event*/):
		heading(heading), body(body)/*, event(event)*/ {}

	std::string getHeading() { return heading; }
	std::optional<std::string> getBody() { return body; }
	//std::optional<time_point> getDue() { return event; }
};
