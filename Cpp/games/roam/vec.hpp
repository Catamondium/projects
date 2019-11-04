#pragma once
#include <curses.h>
#include <cmath>
#include <iostream>
#include <cassert>
#include "pallete.hpp"

struct Vec
{
    int x = 0;
    int y = 0;
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