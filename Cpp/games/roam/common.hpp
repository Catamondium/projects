#include <lua.h>

/// LuaCClosure
/// Identity closure
/// returns (Lua): 1st upvalue
static int l_identity(lua_State *L) {
    lua_pushvalue(L, lua_upvalueindex(1));
    return 1;
}