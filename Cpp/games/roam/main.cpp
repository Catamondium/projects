#include <array>
#include <curses.h>
#include <cstdlib>
#include <time.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "vec.hpp"
#include "player.hpp"
#include "../cursesgame.hpp"
#include "pallete.hpp"

/*
This is not intended as a proper game, this is more of a scratchpad
for implementing a Lua moddable application
*/

#define LUA_ROAM "internal.lua"

struct RoamGame : public CursesGame <2> {
    Player player; // pass to lua via userdata closure?
    lua_State *L;
    int framebase;

    RoamGame() {
        pallete = {
            Color {FOOD, COLOR_GREEN, COLOR_BLACK},
            Color {PLAYER, COLOR_RED, COLOR_BLACK},
        };
        L = luaL_newstate();
        assert(L != NULL);
        luaL_openlibs(L);

        // custom libs
        player.open(L);
        assert(LUA_OK == luaL_dofile(L, LUA_ROAM));
    }

    ~RoamGame() {
	    lua_close(L);
    }

    void init() override {
        setFrameRate(10);
        srand(time(NULL));
        // mocked load procedure, mod locations undetermined
        if (LUA_OK != luaL_dofile(L, "test.lua")) {
            std::cerr << "lua: " << lua_tostring(L, -1) << std::endl;
            throw "Lua exception";
        }
        framebase = lua_gettop(L);
    }

    void loop() override;
    void keyPressed(int) override;
};

void RoamGame::loop()
{
    //noLoop();

    player.lua_serialize(L);

    lua_getglobal(L, "_call_on_tick");
    lua_pcall(L, 0, 0, 0);
    lua_settop(L, framebase);

    player.lua_refresh(L);
}

void RoamGame::keyPressed(int ch)
{
    switch (ch) {
    case 'w':			// UP
    case KEY_UP:
	break;

    case 's':			// RIGHT
    case KEY_DOWN:
	break;

    case 'a':			// LEFT
    case KEY_LEFT:
	break;

    case 'd':			// DOWN
    case KEY_RIGHT:
	break;

    case 'q':			// QUIT
	noLoop();
	break;
    default:
	break;
    }
}

int main()
{
    RoamGame g;
    g.run();
}
