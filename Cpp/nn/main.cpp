#include <unistd.h> // *nix api
#include <stdlib.h>
#include <getopt.h>    // getopt_long
#include <sys/types.h> // *nix types
#include <pwd.h>       // working directory stuff

#include <vector>
#include <memory> // unique_ptr

#include <algorithm> // any_of, remove_if
#include <cctype>    // tolower
#include <filesystem>
namespace fs = std::filesystem;

#include <iostream>
#include <fstream>

#include "lib/parsing.hpp"
#include "lib/note.hpp"
#include "lib/builder.hpp"
#include "lib/util.hpp"
#include "lib/com.hpp"

constexpr int OPTHELP = 500;
const std::string DATAFILE = "/.notes";

/* CLEANUP over summer (OOP integration)
 * builder for stdin mode
 */

void usage(std::string prog)
{
    std::cout << "usage: " << prog << " command [ikhbef]\n"
                                      "options:\n"
                                      //"\t-i\t--interactive Interactive mode\n"
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
    std::vector<Note> notes;
    std::string file = getHome() + DATAFILE;

    NoteBuilder builder;
    std::optional<unsigned int> key;
    std::optional<Note> note;
    bool interactive = false;

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
        /* case 'i':
            interactive = true;
            break;*/
        case 'k':
            key = std::stoi(optarg);
            break;
        case 'h':
            holder = optarg;
            builder.setHeading(util::trim(holder));
            break;
        case 'b':
            holder = optarg;
            builder.setBody(util::trim(holder));
            break;
        case 'e':
            holder = optarg;
            builder.setEvent(util::trim(holder));
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
    notes.erase(std::remove_if(notes.begin(), notes.end(), [](auto x) -> bool { return bool(x); }), notes.end());
    if (!interactive && optind <= argc)
    {
        char ch = (optind != argc) ? std::tolower(argv[optind][0]) : 'l';
        if (std::any_of(COMS.cbegin(), COMS.cend(), [&ch](auto o) -> bool { return ch == o; }))
        {
            Com target = static_cast<Com>(ch);

            std::cout << target << std::endl;
            std::unique_ptr<Command> cmd = comFactory(target, notes, note, key);
            if (cmd == nullptr)
                usage(argv[0]);
            cmd->execute();
        }
        else
            usage(argv[0]);
    } /* else {
        //TODO stdin mode
    }*/

    notelib::unmarshAll(notes, file);
}
