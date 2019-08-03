#pragma once
#include <algorithm> // transform, find_if, remove
#include <iomanip>   // get_time
#include <chrono>    // timepoint

#include "note.hpp"

using systime = std::chrono::system_clock;

namespace notelib
{
std::string ltrim(std::string &s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
                return !std::isspace(ch);
            }));
    return s;
}

std::string rtrim(std::string &s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
                return !std::isspace(ch);
            })
                .base(),
            s.end());
    return s;
}

std::string trim(std::string &s)
{
    ltrim(s);
    rtrim(s);
    return s;
}

std::optional<note_time> makeEvent(std::string value)
{
    std::stringstream ss(value);
    std::tm tmp = {};

    if (value.find('/') != std::string::npos)
    {
        ss >> std::get_time(&tmp, "%d/%m/%Y %R");
    }
    else
    {
        return {};
    }

    return systime::from_time_t(std::mktime(&tmp));
}
} // namespace notelib

struct NoteBuilder
{
    std::string heading = "";
    std::string body = "";
    std::optional<note_time> event = {};

    void setHeading(std::string nheading)
    {
        heading = nheading;
    }

    void setEvent(std::string value)
    {
        event = notelib::makeEvent(value);
    }

    void setBody(std::string nbody)
    {
        body = nbody;
    }

    void addLine(std::string line)
    {
        body += notelib::trim(line) + '\n';
    }

    Note build()
    {
        return Note{heading, notelib::trim(body), event};
    };
};