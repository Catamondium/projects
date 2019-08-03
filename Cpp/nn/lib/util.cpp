#include <algorithm> // transform, find_if, remove
#include <iomanip>   // get_time
#include <chrono>    // timepoint
#include <optional>
#include <string>

#include "util.hpp"
#include "note.hpp"

std::string util::ltrim(std::string &s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
                return !std::isspace(ch);
            }));
    return s;
}

std::string util::rtrim(std::string &s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
                return !std::isspace(ch);
            })
                .base(),
            s.end());
    return s;
}

std::string util::trim(std::string &s)
{
    ltrim(s);
    rtrim(s);
    return s;
}

std::optional<note_time> util::makeEvent(std::string value)
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