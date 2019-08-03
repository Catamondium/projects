// types/containers
#include <vector>
#include <optional>
#include <utility> // pair
#include "note.hpp"
#include "builder.hpp"
// streams
#include <iostream>
#include <fstream>
#include <sstream>
// functional/locale
#include <algorithm> // transform, find_if, remove
#include <locale>    // tolower, isspace

#include "util.hpp"

namespace notelib
{
enum struct Keyword
{
    HEADING,
    EVENT,
    EOE,
    BODY
};

Keyword fEnum(std::string s)
{
    util::rtrim(s);
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);

    if (s == "heading")
        return Keyword::HEADING;

    else if (s == "event")
        return Keyword::EVENT;

    else
        return Keyword::BODY;
}

std::pair<Keyword, int> getkwd(std::string line)
{
    size_t fPos;
    if (line.substr(0, 2) == "##")
        return {Keyword::EOE, -1};

    else if ((fPos = line.find(':')) != std::string::npos)
        return {fEnum(line.substr(0, fPos)), fPos + 1};

    return {Keyword::BODY, -1};
}

std::vector<Note> parse(std::string fname)
{
    std::vector<Note> notes;
    std::string line;
    std::ifstream file(fname);
    NoteBuilder builder;

    while (std::getline(file, line))
    {
        auto [field, pos] = getkwd(line);
        util::trim(line);
        switch (field)
        {
        case Keyword::HEADING:
            line = line.substr(pos);
            builder.setHeading(line);
            break;
        case Keyword::EVENT:
            line = line.substr(pos);
            builder.setEvent(util::ltrim(line));
            break;
        case Keyword::EOE:
            notes.push_back(builder.build());
            builder = NoteBuilder{};
            break;
        default:
            builder.addLine(line);
            break;
        }
    }
    notes.push_back(builder.build());
    return notes;
}

void unmarshAll(std::vector<Note> &notes, std::string fname)
{
    std::ofstream file(fname, std::ofstream::trunc);
    notes.erase(std::remove(notes.begin(), notes.end(), Note()));

    for (Note note : notes)
    {
        file << note.unmarshal() << std::endl;
    }
}
} // namespace notelib
