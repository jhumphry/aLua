-- Lua.Lib
-- Supporting the Lua standard libraries

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Lua.Internal, Lua.AuxInternal, Lua.LibInternal;
use Lua.LibInternal;

package body Lua.Libs is

   function NS(S : String) return chars_ptr renames New_String;

   LibNames : constant array (Lua_Standard_Library) of chars_ptr :=
     (
      Base_Lib => NS(""),
      Package_Lib => NS("package"),
      Coroutine_Lib => NS("coroutine"),
      String_Lib => NS("string"),
      UTF8_Lib => NS("utf8"),
      Table_Lib => NS("table"),
      Math_Lib => NS("math"),
      IO_Lib => NS("io"),
      OS_Lib => NS("os"),
      Debug_Lib => NS("debug"),
      Bit32_Lib => NS("bit32")
     );

   LibFunctions : constant array (Lua_Standard_Library)
     of Lua.Internal.lua_CFunction :=
     (
      Base_Lib => luaopen_base'Access,
      Package_Lib => luaopen_package'Access,
      Coroutine_Lib => luaopen_coroutine'Access,
      String_Lib => luaopen_string'Access,
      UTF8_Lib => luaopen_utf8'Access,
      Table_Lib => luaopen_table'Access,
      Math_Lib => luaopen_math'Access,
      IO_Lib => luaopen_io'Access,
      OS_Lib => luaopen_os'Access,
      Debug_Lib => luaopen_debug'Access,
      Bit32_Lib => luaopen_bit32'Access
     );

   procedure OpenLibs (L : in Lua_State) is
   begin
      LibInternal.luaL_openlibs(L.L);
   end OpenLibs;

   procedure Require_Standard_Library (L : in Lua_State;
                                       Library : in Lua_Standard_Library;
                                       Set_Global : in Boolean := True) is
      glb : constant C.int := C.int((if Set_Global then 1 else 0));
   begin
      Lua.AuxInternal.luaL_requiref(L.L,
                                LibNames(Library),
                                LibFunctions(Library),
                                glb);
   end Require_Standard_Library;

   procedure Add_Yield_Function (L : in Lua_State) is
      Func_Name : C.Strings.chars_ptr := C.Strings.New_String("yield");
   begin
      Internal.lua_pushcclosure(L.L, Yield_CFunction'Access, 0);
      Internal.lua_setglobal(L.L, Func_Name);
      C.Strings.Free(Func_Name);
   end Add_Yield_Function;

   function Yield_CFunction (L : System.Address) return Interfaces.C.int is
   begin
      return Internal.lua_yieldk(L, Internal.lua_gettop(L), 0, null);
   end Yield_CFunction;

end Lua.Libs;
