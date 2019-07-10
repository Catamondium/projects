#include <array>
#include <iostream>
#include <numeric>
#include <optional>
#include <algorithm>
#include <string_view>
#include <memory>

#include "iter_stack.hpp"
#include "../game.hpp"
#include "../cursesgame.hpp"
using Tower = Iter_stack<int>;

std::string printTower(Tower t)
// Printed representation for cmdline
{
    using std::begin;
    using std::end;
    using std::next;
    using std::to_string;

    std::string body;
    if (!t.empty())
        body = std::accumulate(next(begin(t)), end(t), to_string(t[0]), [](std::string s, int a) {
            return std::move(s) + ", " + to_string(a);
        });
    return '[' + body + ']';
}

template <size_t N>
std::string /* Error string */ transfer(std::array<Tower, N> &ts, int from, int to)
// Perform a move
{
    using std::size;
    using std::to_string;

    std::size_t arrsize = size(ts);
    if (from >= arrsize || to >= arrsize)
    {
        return "Bad input";
    }

    std::optional<int> mover, reciever;
    if (!ts[from].empty())
        mover = ts[from].top();
    if (!ts[to].empty())
        reciever = ts[to].top();

    if (mover && reciever && mover > reciever)
    {
        return "Illegal move: " + to_string(mover.value()) + " > " + to_string(reciever.value());
    }

    if (mover)
    {
        ts[to].push(mover.value());
        ts[from].pop();
    }
    else
    {
        return "Illegal move: empty source";
    }

    return "";
}

struct CmdHanoi final : public Game
{
    void init() override
    {
        for (int i = 4; i >= 0; --i)
        {
            towers[0].push(i);
        }

        mov = 0;
        std::cout << "Move? FROM TO";
    }

    void loop() override
    {
        using std::end;

        for (int i = 0; i < towers.size(); ++i)
        {
            std::cout << '[' << i << "]: ";
            std::cout << printTower(towers[i]) << std::endl;
        }

        if (end(towers)->cont() == std::deque{4, 3, 2, 1, 0})
        {
            std::cout << "WIN in " << mov << " moves." << std::endl;
            noLoop();
        }
    }

    void input() override
    {
        int from, to;
        std::cout << mov << " Move? ";
        std::cin >> from;
        std::cin >> to;

        std::string err = transfer(towers, from, to);
        if (err == "")
        {
            ++mov;
        }
        else
        {
            std::cout << err << std::endl;
        }
    }

private:
    std::array<Tower, 3> towers;
    int mov;
};

#ifdef CURSE_ENABLE
struct CursesHanoi : public CursesGame
{
};

std::unique_ptr<Game> chooseHanoi(bool curses)
{
    if (curses)
        return std::make_unique<CursesHanoi>();
    else
        return std::make_unique<CmdHanoi>();
}
#endif

int main(int argc, char **argv)
{
#ifdef CURSE_ENABLE
    bool curses = std::any_of(argv, argv + argc, [](std::string str) {
        return (str.find("curses") != std::string::npos) || (str.find("-c") != std::string::npos);
    });
    std::unique_ptr<Game> hanoi = chooseHanoi(curses);
    hanoi->run();
#else
    CmdHanoi hanoi;
    hanoi.run();
#endif
}