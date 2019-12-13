#include <array>
#include <functional>
#include <curses.h>
#include <cstdlib>
#include <time.h>
#include <optional>

#include <lua5.3/lua.h>
#include <lua5.3/lauxlib.h>
#include <lua5.3/lualib.h>

#include "vec.hpp"
#include "player.hpp"
#include "../cursesgame.hpp"
#include "pallete.hpp"

/*
This is not intended as a proper game, this is more of a scratchpad
for implementing a Lua moddable application
*/

struct RoamGame : public CursesGame <2> {
    Player player; // pass to lua via userdata closure?
    lua_State *L;
    int framebase;

    RoamGame()
    {
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
        std::cout << std::endl;
	    lua_close(L);
    }

    void init() override
    {
        setFrameRate(10);
        srand(time(NULL));
        // mocked load procedure, mod locations undetermined
        LUA_PROTEC(luaL_dofile(L, "test.lua"));
        framebase = lua_gettop(L);
    }

    void lua_event(std::function<void()> event)
    {
        player.lua_serialize(L);
        event();
        lua_settop(L, framebase);
        player.lua_refresh(L);
    }

    void loop() override;
    void keyPressed(int) override;
};

void RoamGame::loop()
{
    lua_getglobal(L, "roam");
    lua_pushinteger(L, width);
    lua_setfield(L, -2, "width");
    lua_pushinteger(L, height);
    lua_setfield(L, -2, "height");

    attron(COLOR_PAIR(PLAYER));
    mvaddch(player.pos.y, player.pos.x, '@');
    attroff(COLOR_PAIR(PLAYER));
    std::cout << "loop" << "\n";
    lua_event([&]{
        lua_getglobal(L, "_call_on_tick");
        LUA_PROTEC(lua_pcall(L, 0, 0, 0));
    });
}

void RoamGame::keyPressed(int ch)
{
    std::optional<Vec> dir;
    switch (ch) {
    case 'w':			// UP
    case KEY_UP:
        dir = Vec{0, -1};
	break;

    case 's':			// RIGHT
    case KEY_DOWN:
        dir = Vec{0, 1};
	break;

    case 'a':			// LEFT
    case KEY_LEFT:
        dir = Vec{-1, 0};
	break;

    case 'd':			// DOWN
    case KEY_RIGHT:
        dir = Vec{1, 0};
	break;

    case 'q':			// QUIT
    case KEY_EXIT:
	noLoop();
	break;

    default:
	break;
    }

    // blocking loop here somehow
    std::cout << "keypressed" << "\n";
    if (dir.has_value()) {
        player.move(dir.value());
        // movement issue is internal, not ncurses
        lua_event([&]{
            lua_getglobal(L, "_call_on_move");
            dir.value().lua_serialize(L);
            LUA_PROTEC(lua_pcall(L, 1, 0, 0));
        });
    }
}

int main()
{
    RoamGame g;
    g.run();
}
