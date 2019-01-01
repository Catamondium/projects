#include <iostream>
#include <vector>
#include <algorithm> // any_of
#include <cctype> // tolower
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

const std::string  DATAFILE = "/.notes";
typedef std::optional<std::string> optstring;
const std::string COMS = "lare";

enum Com : char
{
	LIST   = 'l',
	ADD    = 'a',
	REMOVE = 'r',
	EDIT   = 'e'
};

// convenience overload
std::ostream& operator<<(std::ostream& stream, Com c)
{
	switch(c) {
		case 'l':
			stream << "LS";
			break;
		case 'a':
			stream << "ADD";
			break;
		case 'r':
			stream << "RM";
			break;
		case 'e':
			stream << "EDIT";
	}
	return stream;
}

void execute(Com target, std::optional<Note> note, std::string fname)
{
	std::cout << target << std::endl;
}

void usage(std::string prog) {
	exit(1);
}

std::string getHome()
{
	char *homedir;
	if((homedir = getenv("HOME")) == NULL)
		homedir = getpwuid(getuid())->pw_dir;

	return std::string(homedir);
}

int main(int argc, char **argv)
{
	std::string file = getHome() + DATAFILE;

	optstring head;
	optstring body;
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
			case 'i':
				interactive = true;
				break;
			default:
				usage(argv[0]);
		}
	}

	if(!interactive && optind < argc) { // if interactive, ignore opts
		char c = std::tolower(argv[optind][0]);
		if(std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o){return c == o;})) {
			std::optional<Note> note;
			Com target = static_cast<Com>(c);
			if(head)
				note = Note(head.value(), body, event);
			
			execute(target, note, file);
		} else
			usage(argv[0]);
	} else
		std::cout << "interactive" << std::endl;
}
