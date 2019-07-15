#pragma once
#include <curses.h>
#include <cmath>
#include <iostream>
#include <cassert>
#include "iterable_queue.hpp"
#include "pallete.hpp"

struct Vec
{
    int x;
    int y;
    Vec() = default;
    Vec(int x, int y) : x(x), y(y){};
    Vec operator+(Vec) const;
    bool operator==(Vec o) const
    {
        return x == o.x && y == o.y;
    };

    bool operator!=(Vec o) const
    {
        return !(*this == o);
    }

    bool operator<(Vec o) const
    {
        return x < o.x || y < o.y;
    }

    bool operator>(Vec o) const
    {
        return x > o.x || y > o.y;
    }

    Vec operator-() const
    {
        return Vec{-x, -y};
    };

    operator std::string()
    {
        return "P(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    };
};

Vec spawn(int &width, int &height)
{
    return Vec{rand() % width,
               rand() % height};
}

Vec Vec::operator+(Vec other) const
{
    return Vec{this->x + other.x, this->y + other.y};
}

class Snake
{
    static constexpr char ch = 'O';
    iterable_queue<Vec> body;
    Vec vel;
    bool start = true;
    static constexpr int DEFSIZE = 6;

public:
    inline Vec &head()
    {
        assert(body.size() != 0);
        return body.back();
    }
    Snake() = default;
    Snake(Vec v)
    {
        auto pos = v;
        for (int i = 0; i < DEFSIZE; ++i)
        {
            body.push(v + Vec{i, 0});
        }
    };

    void dir(int x, int y)
    {
        Vec d = Vec{x, y};

        if (vel == -d || (start && vel == Vec{1, 0}))
        {
            start = false;
            return;
        }
        vel = d;
    };

    bool outofbounds(int &width, int &height)
    {
        return (head() < Vec{0, 0} ||
                head() > Vec{width - 1, height - 1});
    }

    bool bodycollide()
    {
        for (auto iter = body.begin(); iter != body.end() - 1; ++iter)
        {
            if (head() == *iter)
                return true;
        }
        return false;
    }

    bool /* dead */ update(Vec &fruit, int &width, int &height)
    {
        if (body.size() == 0)
            return false;

        if (head() == fruit)
        {
            body.push(head() + vel);
            fruit = Vec{spawn(width, height)};
        }

        if (bodycollide())
            return true;

        if (vel != Vec{0, 0})
        {
            Vec h = head() + vel;

            if (outofbounds(width, height))
            {
                return true;
            }

            body.pop();
            body.push(h);
        }

        return false;
    }

    void draw(int, int)
    {
        mvaddstr(0, 0, ("SCORE: " + std::to_string(body.size() - DEFSIZE)).c_str());

        for (Vec &p : body)
        {
            mvaddch(p.y, p.x, ch);
        }
        
        Vec h = head();
        attron(COLOR_PAIR(SNAKE_HEAD));
        mvaddch(h.y, h.x, ch);
        attroff(COLOR_PAIR(SNAKE_HEAD));
    };
};