#pragma once
#include <optional>
#include <chrono> // timepoint

typedef std::chrono::system_clock::time_point note_time;
class Note {
	std::string heading;
	std::optional<std::string> body;
	std::optional<note_time> event;

	public:
	Note(
			std::string heading,
			std::optional<std::string> body,
			std::optional<note_time> event):
		heading(heading), body(body), event(event) {}

	std::string getHeading() { return heading; }
	std::optional<std::string> getBody() { return body; }
	std::optional<note_time> getEvent() { return event; }
	std::optional<std::string> printEvent();
	std::string marshal();
};
