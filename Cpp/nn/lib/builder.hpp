#pragma once

#include "note.hpp"
#include "util.hpp"

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
        event = util::makeEvent(value);
    }

    void setBody(std::string nbody)
    {
        body = nbody;
    }

    void addLine(std::string line)
    {
        body += util::trim(line) + '\n';
    }

    Note build()
    {
        return Note{heading, util::trim(body), event};
    };
};