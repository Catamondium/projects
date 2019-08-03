#include "note.hpp"
#include <iostream>
#include <vector>
#include <memory>

struct Command
{
    virtual void execute() = 0;
};

enum Com : char
{
    LIST = 'l',
    ADD = 'a',
    REMOVE = 'r',
    REPLACE = 'e'
};

const std::string COMS = "lare";

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
    case REPLACE:
        stream << "REPLACE";
    }
    return stream;
}

class List final : public Command
{
    std::vector<Note> &ns;

public:
    List(std::vector<Note> &ns) : ns(ns){};
    const static Com com = LIST;

    void execute() override
    {
        std::cout << "[N]" << std::endl;
        for (unsigned int i = 0; i < ns.size(); ++i)
        {
            std::cout
                << "[" << i << "] "
                << ns[i].unmarshal() << std::endl;
        }
    }
};

class Add final : public Command
{
    std::vector<Note> &ns;
    Note n;

public:
    Add(std::vector<Note> &ns, Note n) : ns(ns), n(n){};
    const static Com com = ADD;

    void execute() override
    {
        ns.push_back(n);
    }
};

class Remove final : public Command
{
    std::vector<Note> &ns;
    unsigned int index;

public:
    Remove(std::vector<Note> &ns, unsigned int index) : ns(ns), index(index){};
    const static Com com = REMOVE;

    void execute() override
    {
        ns.erase(ns.begin() + index);
    }
};

class Replace final : public Command
{
    std::vector<Note> &ns;
    Note n;
    unsigned int index;

public:
    Replace(std::vector<Note> &ns, Note n, unsigned int index) : ns(ns), n(n), index(index){};
    const static Com com = REPLACE;

    void execute() override
    {
        ns[index] = n;
    }
};

std::unique_ptr<Command> comFactory(Com target, std::vector<Note> &ns, std::optional<Note> note, std::optional<unsigned int> index)
{
    switch (target)
    {
    case LIST:
        return std::make_unique<List>(ns);
    case ADD:
        if (note)
        {
            return std::make_unique<Add>(ns, note.value());
        }
        break;
    case REMOVE:
        if (index)
        {
            return std::make_unique<Remove>(ns, index.value());
        }
        break;
    case REPLACE:
        if (note && index)
        {
            return std::make_unique<Replace>(ns, note.value(), index.value());
        }
        break;
    }
    return nullptr;
}