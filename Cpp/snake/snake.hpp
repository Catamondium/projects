#pragma once
#include <curses.h>
#include <cmath>
#include <iostream>
#include <cassert>
#include "iterable_queue.hpp"

struct vec
{
    int x;
    int y;
    vec() = default;
    vec(int x, int y) : x(x), y(y){};
    vec operator+(vec) const;
    bool operator==(vec o) const
    {
        return x == o.x && y == o.y;
    };

    bool operator!=(vec o) const
    {
        return !(*this == o);
    }

    bool operator<(vec o) const
    {
        return x < o.x || y < o.y;
    }

    bool operator>(vec o) const
    {
        return x > o.x || y > o.y;
    }

    vec operator-() const
    {
        return vec{-x, -y};
    };

    operator std::string()
    {
        return "P(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    };
};

vec spawn(int &width, int &height)
{
    return vec{rand() % width,
               rand() % height};
}

vec vec::operator+(vec other) const
{
    return vec{this->x + other.x, this->y + other.y};
}

class snake
{
    static constexpr char ch = 'O';
    iterable_queue<vec> body;
    vec vel;
    bool start = true;

public:
    inline vec &head()
    {
        assert(body.size() != 0);
        return body.back();
    }
    snake() = default;
    snake(vec v)
    {
        auto pos = v;
        for (int i = 0; i < 6; ++i)
        {
            body.push(v + vec{i, 0});
        }
    };

    void dir(int x, int y)
    {
        vec d = vec{x, y};

        if (vel == -d || (start && vel == vec{1, 0}))
        {
            start = false;
            return;
        }
        vel = d;
    };

    bool outofbounds(int &width, int &height)
    {
        return (head() < vec{0, 0} ||
                head() > vec{width - 1, height - 1});
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

    bool /* dead */ update(vec &fruit, int &width, int &height)
    {
        if (body.size() == 0)
            return false;

        if (head() == fruit)
        {
            body.push(head() + vel);
            fruit = vec{spawn(width, height)};
        }

        if (bodycollide())
            return true;

        if (vel != vec(0, 0))
        {
            vec h = head() + vel;

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
        mvaddstr(0, 0, ("SCORE: " + std::to_string(body.size() - 1)).c_str());

        for (vec &p : body)
        {
            mvaddch(p.y, p.x, ch);
        }
    };
};