-- Lua.Util
-- Utility routines to go with the Ada 2012 Lua interface

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

package Lua.Util is

   -- Print the stack out to the console
   procedure Print_Stack(L : in Lua_State'Class);

end Lua.Util;
