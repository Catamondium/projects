#include <iostream>
#include <vector>
#include <algorithm> // any_of
#include <cctype> // tolower
#include <unistd.h> // *nix api
#include <sys/types.h> // *nix types
#include <fstream>
#include <pwd.h> // working directory stuff
#include "lib/parsing.hpp"
#include "lib/note.hpp"

/* TODO
 * IO
 * * argv IO
 * * * COM how ID? In file/vector order??
 * * * list, add, remove, edit
 * * user IO
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
			stream << "LIST";
			break;
		case 'a':
			stream << "ADD";
			break;
		case 'r':
			stream << "REMOVE";
			break;
		case 'e':
			stream << "EDIT";
	}
	return stream;
}

void execute(Com target, std::string fname, std::optional<Note> note, std::optional<unsigned int> index)
{
	std::cout << target << std::endl;
	std::ofstream file(fname, std::ios_base::app);
	std::vector<Note> notes;
	switch(target) {
		case LIST:
			notes = notelib::parse(fname);
			for(auto i = 0; i < notes.size(); i++) {
				std::cout << "[" << i << "] " << notes[i].unmarshal() << std::endl;
			}
			break;
		case ADD:
			if(note)
				file << note.value().unmarshal() << std::endl;
			break;
		case REMOVE:
			notes = notelib::parse(fname);
			if(index && index.value() < notes.size())
				notes.erase(notes.begin() + index.value());
			notelib::unmarshAll(notes, fname);
			break;
		case EDIT:
			notes = notelib::parse(fname);
			if(note && index && index.value() < notes.size())
				notes[index.value()] = note.value();
			notelib::unmarshAll(notes, fname);
			break;
	}
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
	std::optional<unsigned int> index;

	bool user = false; // prompt user by default

	int c;
	while((c = getopt(argc, argv, "ui:h:b:e:")) != -1) {
		std::string holder;
		switch(c) {
			case 'h':
				user = false;
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
			case 'u':
				user = true;
				break;
			case 'i':
				index = std::stoi(optarg);
				break;
			/*case 'f': // Doesn't work with relative paths
				std::cout << "File:\t" << optarg << std::endl;
				file = optarg;
				break;*/
			default:
				usage(argv[0]);
		}
	}

	if(!user && optind < argc) { // if user, ignore opts
		char c = std::tolower(argv[optind][0]);
		if(std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o){return c == o;})) {
			std::optional<Note> note;
			Com target = static_cast<Com>(c);
			if(head)
				note = Note(head.value(), body, event);
			
			execute(target, file, note, index);
		} else
			usage(argv[0]);
	} else
		std::cout << "user" << std::endl;
}
