#include <lua.h>

#define lua_swap(L) \
    lua_rotate((L), -2, 1)


#define LUA_ROAM "internal.lua"

#define LUA_PROTEC(call) \
    do {\
        if (LUA_OK != (call)) {\
            std::cerr << "lua: " << lua_tostring(L, -1) << std::endl;\
            throw "lua exception";\
        }\
    } while(0)