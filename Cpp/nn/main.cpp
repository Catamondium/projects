#include <iostream>
#include <ctime>

class Note {
	std::string title;
	std::string msg;
	std::tm *dueTime;

	public:
	Note(std::string title, std::tm *dueTime, std::string msg):
		title(title), dueTime(dueTime), msg(msg) {}
	std::string getTitle() { return title; }
	std::string getMsg() { return msg; }
	std::tm* getDue() { return dueTime; }
};

std::tm* tm_today() {
	std::time_t today_t = std::time(nullptr);
	std::tm *today = gmtime(&today_t);
	return today;
}

int main(int argc, char** argv) {
	std::tm *today = tm_today();
	Note example("Example", today, "Badminton");
	std::cout << example.getTitle() << std::endl;
	std::cout << asctime(example.getDue()) << std::endl;
	std::cout << example.getMsg() << std::endl;
}
