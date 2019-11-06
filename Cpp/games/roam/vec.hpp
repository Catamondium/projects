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

    Vec operator+(Vec o) const {
        return Vec{x + o.x, y + o.y};
    };

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

    void lua_serialize(lua_State *L) {
        lua_newtable(L);
        lua_pushinteger(L, x);
        lua_setfield(L, -2, "x");
        lua_pushinteger(L, y);
        lua_setfield(L, -2, "y");
    }
};

Vec spawn(int &width, int &height)
{
    return Vec{rand() % width,
               rand() % height};
}