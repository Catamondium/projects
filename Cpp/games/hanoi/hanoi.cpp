#include <array>
#include <deque>
#include <iostream>
#include <numeric>
#include <optional>

#include "iter_stack.hpp"
using Tower = Iter_stack<int>;
using namespace std;

string printTower(Tower t)
// Printed representation for cmdline
{
    string body;
    if (!t.empty())
        body = accumulate(next(t.begin()), t.end(), to_string(t[0]), [](string s, int a) {
            return move(s) + ", " + to_string(a);
        });
    return '[' + body + ']';
}

template <size_t N>
bool /* valid move */ transfer(array<Tower, N> &ts, int from, int to)
// Perform a move
{
    size_t size = ts.size();
    if (from >= size || to >= size)
    {
        cout << "Bad input" << endl;
        return false;
    }

    optional<int> mover, reciever;
    if (!ts[from].empty())
        mover = ts[from].top();
    if (!ts[to].empty())
        reciever = ts[to].top();

    if (mover && reciever && mover > reciever)
    {
        cout << "Illegal move: ";
        cout << mover.value_or(0) << " > " << reciever.value_or(0) << endl;
        return false;
    }

    if (mover)
    {
        ts[to].push(mover.value());
        ts[from].pop();
    }
    else
    {
        cout << "Illegal move: empty source" << endl;
        return false;
    }

    return true;
}

void cmdline()
// Cmdline version of hanoi
// Represented by printing raw stacks
{
    array<Tower, 3> towers;
    for (int i = 4; i >= 0; --i)
    {
        towers[0].push(i);
    }

    int mov = 0;
    while (true)
    {
        for (int i = 0; i < towers.size(); ++i)
        {
            cout << '[' << i << "]: ";
            cout << printTower(towers[i]) << endl;
        }

        int from, to;
        cout << mov << " Move? ";
        cin >> from;
        cin >> to;

        if (transfer(towers, from, to))
        {
            ++mov;
        }

        if (towers[towers.size() - 1].cont() == deque{4, 3, 2, 1, 0})
        {
            cout << "WIN in " << mov << " moves." << endl;
            break;
        }
    }
}

int main()
{
    cmdline();
}