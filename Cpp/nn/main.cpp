#include <iostream>
#include <vector>
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

int main(int argc, char **argv) {
	if(argc < 2) {
		std::cout << "notes file required" << std::endl;
		std::exit(1);
	}

	std::vector<Note> notes = parsing::parse(argv[1]);

	for(auto note : notes) {
		std::cout << "Heading:\t" << note.getHeading() << std::endl;
		std::cout << "\"" << parsing::trim(note.getMsg().value()) << "\"" << std::endl;
	}
}
