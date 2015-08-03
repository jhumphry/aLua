-- Lua.LibInternal
-- Contains the routines for loading the standard Lua library
-- Generated from lualib.h and then manually tidied up

-- Copyright (c) 2015, James Humphry

-- Derived from the original Lua work
-- Copyright (C) 1994-2015 Lua.org, PUC-Rio.

-- See LICENSE.md for details

with Interfaces.C; use Interfaces.C;
with System;

private package Lua.LibInternal is

   function luaopen_base (arg1 : System.Address) return int;  -- /usr/include/lualib.h:15
   pragma Import (C, luaopen_base, "luaopen_base");

   function luaopen_coroutine (arg1 : System.Address) return int;  -- /usr/include/lualib.h:18
   pragma Import (C, luaopen_coroutine, "luaopen_coroutine");

   function luaopen_table (arg1 : System.Address) return int;  -- /usr/include/lualib.h:21
   pragma Import (C, luaopen_table, "luaopen_table");

   function luaopen_io (arg1 : System.Address) return int;  -- /usr/include/lualib.h:24
   pragma Import (C, luaopen_io, "luaopen_io");

   function luaopen_os (arg1 : System.Address) return int;  -- /usr/include/lualib.h:27
   pragma Import (C, luaopen_os, "luaopen_os");

   function luaopen_string (arg1 : System.Address) return int;  -- /usr/include/lualib.h:30
   pragma Import (C, luaopen_string, "luaopen_string");

   function luaopen_utf8 (arg1 : System.Address) return int;  -- /usr/include/lualib.h:33
   pragma Import (C, luaopen_utf8, "luaopen_utf8");

   function luaopen_bit32 (arg1 : System.Address) return int;  -- /usr/include/lualib.h:36
   pragma Import (C, luaopen_bit32, "luaopen_bit32");

   function luaopen_math (arg1 : System.Address) return int;  -- /usr/include/lualib.h:39
   pragma Import (C, luaopen_math, "luaopen_math");

   function luaopen_debug (arg1 : System.Address) return int;  -- /usr/include/lualib.h:42
   pragma Import (C, luaopen_debug, "luaopen_debug");

   function luaopen_package (arg1 : System.Address) return int;  -- /usr/include/lualib.h:45
   pragma Import (C, luaopen_package, "luaopen_package");

  -- open all previous libraries
   procedure luaL_openlibs (arg1 : System.Address);  -- /usr/include/lualib.h:49
   pragma Import (C, luaL_openlibs, "luaL_openlibs");

end Lua.LibInternal;
