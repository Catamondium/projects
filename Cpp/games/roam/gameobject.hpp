#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

struct GameObj {
    /// Build lua closures / tables
    virtual void lua_serialize(lua_State *L);
};