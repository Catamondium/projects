#include <vector>
#include <unordered_map>

#include <algorithm>   // any_of
#include <functional>  // std::function
#include <cctype>      // tolower
#include <filesystem>
namespace fs = std::filesystem;

#include <unistd.h>    // *nix api
#include <sys/types.h> // *nix types
#include <pwd.h>       // working directory stuff

#include <iostream>
#include <fstream>

#include "lib/parsing.hpp"
#include "lib/note.hpp"

/* TODO
 * find a substitute for 'i' to get index
 *
 * IO unification
 * * Tuple passing & concatenation?
 * * * Resolve validity by tuple_size?
 * * * In any case, separate functions from input
 */

const std::string DATAFILE = "/.notes";
const std::string COMS = "lare";

enum Com : char
{
	LIST   = 'l',
	ADD    = 'a',
	REMOVE = 'r',
	EDIT   = 'e'
};

//### COM argv functors
// Ignore related 'unused parameter' warnings
// Modifying signatures will void dispatch ###
bool com_ls(std::string fname, std::optional<Note>, std::optional<unsigned int>)
{
	std::vector<Note> notes = notelib::parse(fname);
	for(unsigned int i = 0; i < notes.size(); ++i) {
		std::cout << "[" << i << "] " << notes[i].unmarshal() << std::endl;
	}
	return false;
}

bool com_add(std::string fname, std::optional<Note> note, std::optional<unsigned int>)
{
	std::ofstream file(fname, std::ios_base::app);
	if(note)
		file << note.value().unmarshal() << std::endl;
	else
		return true;
	return false;
}

bool com_rm(std::string fname, std::optional<Note>, std::optional<unsigned int> index)
{
	std::vector<Note> notes = notelib::parse(fname);
	if(index && index.value() < notes.size())
		notes.erase(notes.begin() + index.value());
	else
		return true;
	notelib::unmarshAll(notes, fname);
	return false;
}

bool com_edit(std::string fname, std::optional<Note> note, std::optional<unsigned int> index)
{
	std::vector<Note> notes = notelib::parse(fname);
	if(note && index && index.value() < notes.size())
		notes[index.value()] = note.value();
	else
		return true;
	notelib::unmarshAll(notes, fname);
	return false;
}
//###

using com_functor = std::function<bool/*HasError*/(std::string, std::optional<Note>, std::optional<unsigned int>)>;
std::unordered_map<Com, com_functor> dispatch
{
	{LIST,   com_ls},
	{ADD,    com_add},
	{REMOVE, com_rm},
	{EDIT,   com_edit}
};

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

unsigned int i_index(unsigned int size)
{
	unsigned int index;
	do {
		std::cout << "Select N: ";
		std::cin >> index;
	} while(index >= size);

	return index;
}

Note i_note()
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

void i_list(std::vector<Note> &notes)
{
	std::cout << "[N] interactive" << std::endl;
	for(unsigned int i = 0; i < notes.size(); ++i) {
		std::cout << "[" << i << "] " << notes[i].unmarshal() << std::endl;
	}
}

void usage(std::string prog)
{
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
	std::optional<unsigned int> index;

	bool interactive = false;

	int c;
	while((c = getopt(argc, argv, "ui:h:b:e:f:")) != -1) {
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
			case 'u': // make use i
				interactive = true;
				break;
			case 'i': // replace with something intuitive, 'element'?
				index = std::stoi(optarg);
				break;
			case 'f':
				std::cout << "File:\t" << optarg << std::endl;
				file = fs::absolute(optarg).string();
				break;
			default:
				usage(argv[0]);
		}
	}

	std::optional<Note> note;
	if(!interactive && optind < argc) {
		char c = std::tolower(argv[optind][0]);
		if(std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o){return c == o;})) {
			Com target = static_cast<Com>(c);
			if(head)
				note = Note(head.value(), body, event);
			
			std::cout << target << std::endl;
			if(dispatch[target](file, note, index))
				usage(argv[0]);
		} else
			usage(argv[0]);
	} else {
		std::vector<Note> notes = notelib::parse(file);
		i_list(notes);
		std::cout << "Actions: Add, Remove, Edit" << std::endl;
		const std::string i_COMS = COMS.substr(1);
		char action;
		do {
			std::cout << "Select action: ";
			std::cin >> action;
			action = std::tolower(action);
		} while (std::none_of(i_COMS.cbegin(), i_COMS.cend(),
					[&action](auto o){return action == o;}));
		Com command = static_cast<Com>(action);
		std::cout << '\n' << command << std::endl;
		
		if(command == REMOVE || command == EDIT) {
			unsigned int index = i_index(notes.size());
			if(command == REMOVE)
				notes.erase(notes.begin() + index);
			else
				notes[index] = i_note();
		} else
			notes.push_back(i_note());
		notelib::unmarshAll(notes, file);
		std::cout << std::endl; //spacing
		i_list(notes);

		/* Interactive IDEAS
		 * Loop until interrupt?
		 */
	}
}
