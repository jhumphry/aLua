-- Lua.Lib
-- Supporting the Lua standard libraries

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Strings, Ada.Strings.Fixed;

with Lua.Internal, Lua.AuxInternal, Lua.LibInternal;
use Lua.LibInternal;

package body Lua.Libs is

   subtype Lib_Name is String(1..9);

   LibNames : constant array (Lua_Standard_Library)
     of Lib_Name :=
     (
      Base_Lib => "         ",
      Package_Lib => "package  ",
      Coroutine_Lib => "coroutine",
      String_Lib => "string   ",
      UTF8_Lib => "utf8     ",
      Table_Lib => "table    ",
      Math_Lib => "math     ",
      IO_Lib => "io       ",
      OS_Lib => "os       ",
      Debug_Lib => "debug    ",
      Bit32_Lib => "bit32    "
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
      C_libname : C.Strings.chars_ptr
        := C.Strings.New_String(Ada.Strings.Fixed.Trim(LibNames(Library),
                                Ada.Strings.Right));
   begin
      Lua.AuxInternal.luaL_requiref(L.L,
                                    C_libname,
                                    LibFunctions(Library),
                                    glb);
      C.Strings.Free(C_libname);
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
