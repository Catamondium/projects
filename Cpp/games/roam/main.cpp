#include <array>
#include <curses.h>
#include <cstdlib>
#include <time.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "vec.hpp"
#include "../cursesgame.hpp"
#include "pallete.hpp"

/*
This is not intended as a proper game, this is more of a 'scratch'
for implementing a Lua modding
*/

struct RoamGame : public CursesGame<2>
{
    Vec fruit;

    RoamGame()
    {
        pallete = {
            Color{FOOD, COLOR_GREEN, COLOR_BLACK},
            Color{PLAYER, COLOR_RED, COLOR_BLACK},
        };
    }

    void init() override
    {
        setFrameRate(10);
        srand(time(NULL));
    }

    void loop() override;
    void keyPressed(int) override;
};

void RoamGame::loop()
{
}

void RoamGame::keyPressed(int ch)
{
    switch (ch)
    {
    case 'w': // UP
    case KEY_UP:
        break;

    case 's': // RIGHT
    case KEY_DOWN:
        break;

    case 'a': // LEFT
    case KEY_LEFT:
        break;

    case 'd': // DOWN
    case KEY_RIGHT:
        break;

    case 'q': // QUIT
        noLoop();
        break;
    default:
        break;
    }
}

int main()
{
    //RoamGame g;
    //g.run();

    lua_State *L = luaL_newstate();
    luaL_openlibs(L); // loads lua standard lib

    // 5 + 5
    lua_Integer a = 5;
    lua_Integer b = 5;
    lua_pushinteger(L, a); // Push params to stack
    lua_pushinteger(L, b); // Push params to stack
    lua_arith(L, LUA_OPADD); // Perform binary ADD, pops 2, pushes 1
    int ret = lua_tointeger(L, -1); // read top of stack, DOESN'T POP
    lua_pop(L, 1); // clean stack
    std::cout << "Lua " << a << " + " << b << " == " << ret << std::endl;

    lua_close(L);
}
