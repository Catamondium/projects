#include <iostream>
#include <vector>
#include <unistd.h> // *nix api
#include <sys/types.h> // *nix types
#include <pwd.h> // working directory stuff
#include "lib/parsing.hpp"
#include "lib/note.hpp"
#define  DATAFILE "/.notes"

/* TODO
 * IO
 * * implement argv IO
 * * implement interactive IO
 */

std::string getHome() {
	char *homedir;
	if((homedir = getenv("HOME")) == NULL)
		homedir = getpwuid(getuid())->pw_dir;

	return std::string(homedir);
}

int main(/*int argc, char **argv*/) {
	std::cout << "Unmarshalling:\t" <<  getHome() + DATAFILE << std::endl;

	std::vector<Note> notes = notelib::parse(getHome() + DATAFILE);

	std::cout << "Marshalling:\t" << "marsh_notes" << std::endl;
	notelib::unmarshAll(notes, "marsh_notes");
	/*for(auto note : notes) {
		std::cout << note.marshal() << std::endl;
	}*/
}
