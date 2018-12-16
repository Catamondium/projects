#pragma once
#include <optional>
//#include <chrono> // time_point, format

class Note {
	std::string heading;
	//std::optional<std::tm*> dueTime;
	std::optional<std::string> msg;

	public:
	Note(
			std::string heading,
			std::optional<std::string> msg/*,
			std::optional<std::tm*> dueTime*/):
		heading(heading), msg(msg) {
			//dueTime = dueTime;
		}

	std::string getHeading() { return heading; }
	std::optional<std::string> getMsg() { return msg; }
	//std::optional<std::tm*> getDue() { return dueTime; }
};
