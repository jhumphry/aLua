-- Lua.Lib
-- Supporting the Lua standard libraries

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Finalization;

private with Interfaces.C.Strings;

package Lua.Libs is

   type Lua_Standard_Library is (Base_Lib,
                                 Package_Lib,
                                 Coroutine_Lib,
                                 String_Lib,
                                 UTF8_Lib,
                                 Table_Lib,
                                 Math_Lib,
                                 IO_Lib,
                                 OS_Lib,
                                 Debug_Lib,
                                 Bit32_Lib);

   -- Ensure that all standard Lua libraries are available
   procedure OpenLibs (L : in Lua_State);

   -- Ensure that a particular standard Lua library is available
   procedure Require_Standard_Library (L : in Lua_State;
                                       Library : in Lua_Standard_Library;
                                       Set_Global : in Boolean := True);

   -- This will add a Yield function to a Lua state for occasions where you
   -- do not wish to add the whole Coroutine library
   procedure Add_Yield_Function (L : in Lua_State);

private

   function Yield_CFunction (L : System.Address) return Interfaces.C.int
     with Convention => C;

end Lua.Libs;
