#pragma once

#include <curses.h>
#include <cmath>
#include <iostream>
#include "iterable_queue.h"

struct vec
{
    int x;
    int y;
    vec() = default;
    vec(int x, int y) : x(x), y(y){};
    vec operator+(vec) const;
    bool operator==(vec o) const
    {
        return this->x == o.x && this->y == o.y;
    };

    bool operator!=(vec o) const
    {
        return !(*this == o);
    }

    vec operator-() const
    {
        return {-x, -y};
    };

    operator std::string()
    {
        return "P(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    };
};

vec vec::operator+(vec other) const
{
    return {this->x + other.x, this->y + other.y};
}

class snake
{
    static constexpr char ch = 'O';
    // NOTE size >= 1 currently
    iterable_queue<vec> body;
    vec vel;
    inline vec &head() { return body.front(); }

public:
    snake() = default;
    snake(int width, int height)
    {
        body.push({(int)std::floor(rand() % width),
                   (int)std::floor(rand() % height)});
    };

    void dir(int x, int y)
    {
        vec d = {x, y};
#ifdef DEBUG
        if (vel == -d)
            return;
#endif
        vel = d;
    };

    void update()
    {
        if (vel != vec(0, 0))
        {
            vec h = head() + vel;
            body.pop();
            body.push(h);
        }
    }

    void draw(int, int)
    {
        update();

        mvprintw(0, 2, ("SCORE: " + std::to_string(body.size())).c_str());
#ifdef DEBUG
        mvprintw(10, 0, std::string(head()).c_str());
#endif

        for (vec &p : body)
        {
            mvaddch(p.y, p.x, ch);
        }
    };
};