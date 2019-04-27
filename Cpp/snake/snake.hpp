#pragma once
#include <curses.h>
#include <cmath>
#include <iostream>
#include "iterable_queue.hpp"

enum snakestate
{
    NIL,
    EATEN,
    TAIL,
    WALL,
};

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

#ifdef DEBUG
    operator std::string()
    {
        return "P(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    };
#endif
};

vec spawn(int &width, int &height)
{
    return vec{(int)std::floor(rand() % width),
               (int)std::floor(rand() % height)};
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
    bool wallwrap;
    inline vec &head() { return body.back(); }

public:
    snake() = default;
    snake(vec v, bool wallwrap) : wallwrap(wallwrap)
    {
        body.push(v);
    };

    void dir(int x, int y)
    {
        vec d = vec{x, y};
        
        if (vel == -d)
            return;
        vel = d;
    };

    bool outofbounds(int &width, int &height)
    {
        return (head() < vec{0, 0} ||
                head() > vec{width - 1, height - 1});
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

        // Body collision
        for (auto iter = body.begin(); iter != body.end() - 1; ++iter)
        {
            if (head() == *iter)
                return true;
        }

        if (vel != vec(0, 0))
        {
            vec h = head() + vel;

            if (wallwrap)
            {
                h.x %= width;
                h.y %= height;
            }
            else if (outofbounds(width, height))
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