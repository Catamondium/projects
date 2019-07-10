#include <array>
#include <algorithm>
#include <chrono>
using namespace std::chrono_literals;
#include <curses.h>
#include <utility>
#include <sstream>

#include "hanoi.hpp"
#include "../cursesgame.hpp"

constexpr int odd(int n)
{
    return (2 * n) + 1;
}

struct CursesHanoi final : public CursesGame
{
    static constexpr char ch = '#';
    static constexpr int maxval = 4;
    static constexpr int realmax = odd(maxval);

    void init() override;
    void loop() override;
    void input() override;

private:
    std::array<Tower, 3> towers;
    int mov = 0;
    std::string err = "";
};

std::pair<std::string, int> drawTower(Tower t)
{
    if (t.empty())
        return std::make_pair("", 0);

    int max = *std::max_element(t.begin(), t.end());
    int lines = 0;
    std::string out;
    for (auto it = t.rbegin(); it != t.rend(); ++it)
    {
        int spaces = (max - *it) / 2;
        out += std::string(spaces, ' ') + std::string(*it, CursesHanoi::ch);

        out += "\n";
        lines++;
    }

    return std::make_pair(out, lines);
}

void CursesHanoi::init()
{
    timeout(-1); // Turn blocking IO back on
    echo();      // User sees input
    frameperiod = 0s;

    for (int i = maxval; i >= 0; --i)
    {
        towers[0].push(odd(i));
    }

    for (int i = 1; i < towers.size(); ++i)
    {
        towers[i] = Tower();
    }
}

void CursesHanoi::loop()
{
    using std::to_string;

    mvaddstr(0, 0, err.c_str());
    for (int i = 0; i < towers.size(); ++i)
    {
        auto [repr, lines] = drawTower(towers[i]);
        int corner_y = height - lines - 1;
        int corner_x = (i == 0) ? 0 : (i * realmax) + 2;
        mvaddstr(corner_y - 1, corner_x, to_string(i).c_str());
        render(corner_y, corner_x, repr);
    }

    if (towers[towers.size() - 1].cont() == std::deque<int>{odd(4), odd(3), odd(2), odd(1), odd(0)})
    {
        mvaddstr(height / 2, width / 2, ("WIN in " + to_string(mov) + " moves.").c_str());
        mvaddstr((height / 2) + 1, width / 2, "Ctl + C to quit");
        noecho();
        int ch = getch();
    }
}

void CursesHanoi::input()
{
    mvaddstr(height - 1, 0, "Move? ");

    char input[width];
    getnstr(input, width);
    std::stringstream ss(input);

    int from, to;
    ss >> from;
    ss >> to;

    err = transfer(towers, from, to);
    if (err == "")
    {
        mov++;
    }
}

int main()
{
    CursesHanoi hanoi;
    hanoi.run();
}