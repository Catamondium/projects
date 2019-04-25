#include <unistd.h> // *nix api
#include <stdlib.h>
#include <getopt.h>	// getopt_long
#include <sys/types.h> // *nix types
#include <pwd.h>	   // working directory stuff

#include <vector>

#include <algorithm>  // any_of
#include <functional> // std::function
#include <cctype>	 // tolower
#include <csignal>
#include <filesystem>
namespace fs = std::filesystem;

#include <iostream>
#include <fstream>

#include "lib/parsing.hpp"
#include "lib/note.hpp"

constexpr int OPTHELP = 500;
const std::string DATAFILE = "/.notes";
const std::string COMS = "lare";

// lucky we need all these in main
std::string file;
std::vector<Note> notes;

enum Com : char
{
	LIST = 'l',
	ADD = 'a',
	REMOVE = 'r',
	EDIT = 'e'
};

namespace com
{
void ls(std::vector<Note> &ns)
{
	std::cout << "[N]" << std::endl;
	for (unsigned int i = 0; i < ns.size(); ++i)
	{
		std::cout << "[" << i << "] "
				  << ns[i].unmarshal() << std::endl;
	}
}

void add(std::vector<Note> &ns, Note n)
{
	ns.push_back(n);
}

void rm(std::vector<Note> &ns, unsigned int key)
{
	ns.erase(ns.begin() + key);
}

void edit(std::vector<Note> &ns, Note n, unsigned int key)
{
	ns[key] = n;
}
} // namespace com

bool /*hasError*/ execute(Com target, std::vector<Note> &ns, std::optional<Note> note, std::optional<unsigned int> index)
{
	switch (target)
	{
	case LIST:
		com::ls(ns);
		return false;
	case ADD:
		if (note)
		{
			com::add(ns, note.value());
			return false;
		}
		break;
	case REMOVE:
		if (index)
		{
			com::rm(ns, index.value());
			return false;
		}
		break;
	case EDIT:
		if (note && index)
		{
			com::edit(ns, note.value(), index.value());
			return false;
		}
		break;
	}

	return true;
}

std::ostream &operator<<(std::ostream &stream, Com c)
{
	switch (c)
	{
	case LIST:
		stream << "LIST";
		break;
	case ADD:
		stream << "ADD";
		break;
	case REMOVE:
		stream << "REMOVE";
		break;
	case EDIT:
		stream << "EDIT";
	}
	return stream;
}

void usage(std::string prog)
{
	std::cout << "usage: " << prog << " command [ikhbef]\n"
									  "options:\n"
									  "\t-k\t--key Key of note\n"
									  "\t-h\t--heading Define heading\n"
									  "\t-b\t--body Define body\n"
									  "\t-e\t--event Define event\n"
									  "\t-f\t--file Set subject file\n"
									  "\t--help\tPrint this message and exit\n"
									  "command:\n"
									  "\tlist:\tlist notes in file\n"
									  "\tadd:\tadd note to file\n"
									  "\tremove:\tremove note from file\n"
									  "\tedit:\tedit note by replacement"
			  << std::endl;
	exit(1);
}

std::string getHome()
{
	char *homedir;
	if ((homedir = getenv("HOME")) == NULL)
		homedir = getpwuid(getuid())->pw_dir;

	return std::string(homedir);
}

int main(int argc, char **argv)
{
	std::string file = getHome() + DATAFILE;

	std::string head = "";
	std::string body = "";
	std::optional<note_time> event;
	std::optional<unsigned int> key;
	std::optional<Note> note;

	static struct option long_options[] = {
		//{"interactive", no_argument,       0, 'i'},
		{"key", required_argument, 0, 'k'},
		{"header", required_argument, 0, 'h'},
		{"body", required_argument, 0, 'b'},
		{"event", required_argument, 0, 'e'},
		{"file", required_argument, 0, 'f'},
		{"help", no_argument, 0, OPTHELP}};

	int c;
	int option_index = 0;
	while ((c = getopt_long(argc, argv, "k:h:b:e:f:",
							long_options, &option_index)) != -1)
	{
		std::string holder;
		switch (c)
		{
		case 'h':
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
		case 'k':
			key = std::stoi(optarg);
			break;
		case 'f':
			std::cout << "File:\t" << optarg << std::endl;
			file = fs::absolute(optarg).string();
			break;
		default:
			usage(argv[0]);
		}
	}

	note = Note(head, body, event);

	notes = notelib::parse(file);
	notes.erase(std::remove(notes.begin(), notes.end(), Note()));
	if (optind < argc)
	{
		char c = std::tolower(argv[optind][0]);
		if (std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o) { return c == o; }))
		{
			Com target = static_cast<Com>(c);

			std::cout << target << std::endl;
			if (execute(target, notes, note, key))
				usage(argv[0]);
		}
		else
			usage(argv[0]);
	} /* else {
		com::ls(notes);
		// should only exist when notes are initialised
		signal(SIGINT, inthandler); 
		while(true) {
			Com command = iutil::com();
			std::cout << '\n' << command << std::endl;
			
			switch(command) {
				case LIST:
					com::ls(notes);
					break;
				case ADD:
					com::add(notes, iutil::note());
					break;
				case REMOVE:
					com::rm(notes, iutil::key(notes.size()));
					break;
				case EDIT:
					com::edit(notes, iutil::note(), iutil::key(notes.size()));
					break;
			}
			std::cout << std::endl; //spacing
			// clean
			notes.erase(std::remove(notes.begin(), notes.end(), Note()));
			com::ls(notes);
		}
	}*/

	notelib::unmarshAll(notes, file);
}
