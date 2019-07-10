#include <array>
#include <numeric>
#include <optional>

#include "iter_stack.hpp"
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