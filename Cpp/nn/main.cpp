#include <unistd.h>    // *nix api
#include <stdlib.h>
#include <getopt.h>    // getopt_long
#include <sys/types.h> // *nix types
#include <pwd.h>       // working directory stuff

#include <vector>
#include <unordered_map>

#include <algorithm>   // any_of
#include <functional>  // std::function
#include <cctype>      // tolower
#include <filesystem>
namespace fs = std::filesystem;

#include <iostream>
#include <fstream>

#include "lib/parsing.hpp"
#include "lib/note.hpp"

constexpr int OPTHELP = 500;
const std::string DATAFILE = "/.notes";
const std::string COMS = "lare";

enum Com : char
{
	LIST   = 'l',
	ADD    = 'a',
	REMOVE = 'r',
	EDIT   = 'e'
};

namespace com {
	void ls(std::vector<Note> &notes)
	{
		std::cout << "[N]" << std::endl;
		for(unsigned int i = 0; i < notes.size(); ++i) {
			std::cout << "[" << i << "] "
				<< notes[i].unmarshal() << std::endl;
		}
	}

	void add(std::vector<Note> &notes, Note n)
	{
		notes.push_back(n);
	}

	void rm(std::vector<Note> &notes, unsigned int key)
	{
		notes.erase(notes.begin() + key);
	}

	void edit(std::vector<Note> &notes, Note n, unsigned int key)
	{
		notes[key] = n;
	}
}

bool/*hasError*/ execute(Com target, std::vector<Note> &notes, std::optional<Note> note, std::optional<unsigned int> index)
{
	switch(target) {
		case LIST:
			com::ls(notes);
			return true;
		case ADD:
			if(note) {
				com::add(notes, note.value());
				return true;
			}
			break;
		case REMOVE:
			if(index) {
				com::rm(notes, index.value());
				return true;
			}
			break;
		case EDIT:
			if(note && index) {
				com::edit(notes, note.value(), index.value());
				return true;
			}
			break;
	}

	return false;
}

std::ostream& operator<<(std::ostream& stream, Com c)
{
	switch(c) {
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

namespace iutil { // interactive side parameter aquisition
	unsigned int key(unsigned int size)
	{
		unsigned int key;
		do {
			std::cout << "Select N: ";
			std::cin >> key;
		} while(key >= size);

		return key;
	}

	Note note()
	{
		std::string head;
		std::cout << "Heading: ";
		std::cin.ignore();
		std::getline(std::cin, head);
		notelib::trim(head);

		std::string strdate;
		std::cout << "Event: ";
		std::cin.ignore(0);
		std::getline(std::cin, strdate);
		notelib::trim(strdate);
		std::optional<note_time> event = notelib::makeEvent(strdate);

		std::string body = ""; 
		std::string cur;
		std::cout << "Body: finalise with \"##\"" << std::endl;
		std::cin.ignore(0);
		do {
			std::getline(std::cin, cur);
			notelib::trim(cur);
			body += cur + '\n';
		} while(cur.substr(0, 2) != "##"); // stop reading on EOE

		notelib::trim(body);
		body.erase(std::find_if(body.rbegin(), body.rend(),
					[](char ch){return ch == '\n';}).base()-1, body.end()); // remove EOE line

		return Note(head, body, event);
	}

	Com com()
	{
		std::cout << "Actions: Add, Remove, Edit" << std::endl;
		char action;
		do {
			std::cout << "Select action: ";
			std::cin >> action;
			action = std::tolower(action);
		} while (std::none_of(COMS.cbegin()+1, COMS.cend(),
					[&action](auto o){return action == o;}));
		return static_cast<Com>(action);
	}
}

void usage(std::string prog)
{
	std::cout <<
		"usage: " << prog << " command [ikhbef]\n"
		"options:\n"
		"\t-i\t--interactive\n"
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
		"\tedit:\tedit note by replacement" << std::endl;
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

	std::optional<std::string> head;
	std::string body = "";
	std::optional<note_time> event;
	std::optional<unsigned int> key;
	std::optional<Note> note;

	bool interactive = false;

	static struct option long_options[] = {
		{"interactive", no_argument,       0, 'i'},
		{"key",         required_argument, 0, 'k'},
		{"header",      required_argument, 0, 'h'},
		{"body",        required_argument, 0, 'b'},
		{"event",       required_argument, 0, 'e'},
		{"file",        required_argument, 0, 'f'},
		{"help",        no_argument,       0, OPTHELP}
	};

	int c;
	int option_index = 0;
	while((c = getopt_long(argc, argv, "ik:h:b:e:f:",
					long_options, &option_index)) != -1) {
		std::string holder;
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

	if(head)
		note = Note(head.value(), body, event);

	std::vector<Note> notes = notelib::parse(file);
	if(!interactive && optind < argc) {
		char c = std::tolower(argv[optind][0]);
		if(std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o){return c == o;})) {
			Com target = static_cast<Com>(c);
			
			std::cout << target << std::endl;
			if(execute(target, notes, note, key))
				usage(argv[0]);
		} else
			usage(argv[0]);
	} else {
		com::ls(notes);
		Com command = iutil::com();
		std::cout << '\n' << command << std::endl;
		
		if(command == REMOVE || command == EDIT) {
			if(command == REMOVE)
				com::rm(notes, iutil::key(notes.size()));
			else
				com::edit(notes, iutil::note(), iutil::key(notes.size()));
		} else
			com::add(notes, iutil::note());
		std::cout << std::endl; //spacing
		com::ls(notes);
	}
	notelib::unmarshAll(notes, file);
}
