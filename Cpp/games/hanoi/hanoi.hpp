#include <array>
#include <numeric>
#include <optional>
#include <string>

#include "iter_stack.hpp"
using Tower = Iter_stack<int>;

constexpr int TOWERS = 3;
constexpr int MAXVAL = 4;

struct Error
{
    enum kinds
    {
        NONE,
        BAD_INPUT,
        ILL_COMP,
        ILL_EMPTY
    } kind = NONE;
    std::string str = "";
    Error() = default;
    Error(kinds kind, std::string str) : kind(kind), str(str) {}
    operator bool()
    {
        return kind != NONE;
    }
};

std::string
printTower(Tower t)
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
Error transfer(std::array<Tower, N> &ts, int from, int to)
// Perform a move
{
    using std::size;
    using std::to_string;

    std::size_t arrsize = size(ts);
    if (from >= arrsize || to >= arrsize)
    {
        return {Error::kinds::BAD_INPUT, "Bad input"};
    }

    std::optional<int> mover, reciever;
    if (!ts[from].empty())
        mover = ts[from].top();
    if (!ts[to].empty())
        reciever = ts[to].top();

    if (mover && reciever && mover > reciever)
    {
        return {Error::kinds::ILL_COMP, "Illegal move: " + to_string(mover.value()) + " > " + to_string(reciever.value())};
    }

    if (mover)
    {
        ts[to].push(mover.value());
        ts[from].pop();
    }
    else
    {
        return {Error::kinds::ILL_EMPTY, "Illegal move: empty source"};
    }

    return {};
}