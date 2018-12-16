#include <iostream>
#include <vector>
#include <unistd.h> // *nix api
#include <sys/types.h> // *nix types
#include <pwd.h> // working directory stuff
#include "parsing.hpp"
#include "note.hpp"

/* TODO
 * Decide on DueTime implementation
 * * Implement looped parsing for the field
 * IO
 * * implement argv IO
 * * implement interactive IO
 * Notification system
 */

std::string getHome() {
	char *homedir;
	if((homedir = getenv("HOME")) == NULL)
		homedir = getpwuid(getuid())->pw_dir;

	return std::string(homedir);
}

int main(int argc, char **argv) {
	std::cout << getHome() + "/.notes" << std::endl;
	/*if(argc < 2) {
		std::cout << "notes file required" << std::endl;
		std::exit(1);
	}*/

	std::vector<Note> notes = parsing::parse(getHome()+"/.notes");//argv[1]);

	for(auto note : notes) {
		std::cout << "Heading:\t" << note.getHeading() << std::endl;
		std::cout << "\"" << parsing::trim(note.getMsg().value()) << "\"" << std::endl;
	}
}
