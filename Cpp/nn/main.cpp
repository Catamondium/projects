#include <unistd.h> // *nix api
#include <stdlib.h>
#include <getopt.h>    // getopt_long
#include <sys/types.h> // *nix types
#include <pwd.h>       // working directory stuff

#include <vector>
#include <memory> // unique_ptr

#include <algorithm>  // any_of, remove_if
#include <functional> // std::function
#include <cctype>     // tolower
#include <csignal>
#include <filesystem>
namespace fs = std::filesystem;

#include <iostream>
#include <fstream>

#include "lib/parsing.hpp"
#include "lib/note.hpp"
#include "lib/builder.hpp"
#include "lib/com.hpp"
#include <cassert>

constexpr int OPTHELP = 500;
const std::string DATAFILE = "/.notes";

// lucky we need all these in main
std::string file;
std::vector<Note> notes;

/* CLEANUP over summer (OOP integration)
    * Builder pattern
    * * use for usr input
    * * * pick between cin and readline(), else problems
*/

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

    //std::string head = "";
    //std::string body = "";
    //std::optional<note_time> event;
    NoteBuilder builder;
    std::optional<unsigned int> key;
    std::optional<Note> note;

    static const option long_options[] = {
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
            builder.setHeading(notelib::trim(holder));
            break;
        case 'b':
            holder = optarg;
            builder.setBody(notelib::trim(holder));
            break;
        case 'e':
            holder = optarg;
            builder.setEvent(notelib::trim(holder));
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

    note = builder.build();

    notes = notelib::parse(file);
    // TODO: filter out blanks safely
    if (optind <= argc)
    {
        char c = (optind != argc) ? std::tolower(argv[optind][0]) : 'l';
        if (std::any_of(COMS.cbegin(), COMS.cend(), [&c](auto o) -> bool { return c == o; }))
        {
            Com target = static_cast<Com>(c);

            std::cout << target << std::endl;
            std::unique_ptr<Command> cmd = comFactory(target, notes, note, key);
            cmd->execute();
            if (cmd == nullptr)
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
