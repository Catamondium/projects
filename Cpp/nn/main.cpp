#include <iostream>
#include <vector>
#include <unistd.h> // *nix api
#include <sys/types.h> // *nix types
#include <pwd.h> // working directory stuff
#include "lib/parsing.hpp"
#include "lib/note.hpp"

/* TODO
 * IO
 * * argv IO
 * * * COM how ID? In file/vector order??
 * * * list, add, remove, edit
 * * interactive IO
 * * * Traverse by ID, parse com by letter
 * * callable COM struct?
 */

enum Com {LIST, ADD, REMOVE, EDIT};

const std::string  DATAFILE = "/.notes";

std::string getHome()
{
	char *homedir;
	if((homedir = getenv("HOME")) == NULL)
		homedir = getpwuid(getuid())->pw_dir;

	return std::string(homedir);
}

int main(int argc, char **argv)
{
	std::optional<std::string> head;
	std::optional<std::string> body;
	std::optional<note_time> event;

	bool interactive = true; // prompt user by default

	int c;
	std::string holder;
	while((c = getopt(argc, argv, "ih:b:e:")) != -1) {
		switch(c) {
			case 'h':
				interactive = false;
				holder = optarg;
				head = notelib::trim(holder);
				break;
			case 'b':
				holder = optarg;
				body = notelib::trim(holder);
				break;
			case 'e':
				holder = optarg;
				event = notelib::makeEvent(notelib::trim(holder));
				break;
		}
	}

	if(!interactive) { // if interactive, ignore opts
		if(head) { // all notes require header
			std::cout << head.value() << std::endl;
			if(body)
				std::cout << body.value() << std::endl;
			if(event)
				std::cout << "Event parsed" << std::endl;
		} // else noop
	} else
			std::cout << "interactive" << std::endl;
}
