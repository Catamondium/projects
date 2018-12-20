#pragma once
#include <optional>
#include <sstream>
#include <iomanip>
#include <chrono> // std::chrono::sys_time

typedef std::chrono::milliseconds note_duration;
typedef std::chrono::time_point<std::chrono::system_clock, note_duration> note_time;
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
	//std::optional<std::string> printEvent(); // Currently disfunctional
};

// Converting note_time to string?
// simple solutions appear based on std::chrono::system_time
// but no available duration type for cast????
/*std::optional<std::string> Note::printEvent() {
	if(!event) return {};
	std::chrono::system_clock::time_point t;

	t = std::chrono::time_point_cast<decltype(t.time_since_epoch())>(event);
	std::time_t tt = std::chrono::system_clock::to_time_t(t);

	struct std::tm *tm = std::localtime(&tt);
	std::stringstream ss;
	ss << std::put_time(tm, "%d/%m/%Y %R");
	return ss.str();
}*/
