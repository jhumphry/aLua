-- Lua.LibInternal

-- Contains the routines for loading the standard Lua library
-- Generated from lualib.h and then manually tidied up

-- Copyright (c) 2015, James Humphry
-- under the same terms as the original source

-- The copyright notice of the original source follows:
--*****************************************************************************
--* Copyright (C) 1994-2015 Lua.org, PUC-Rio.
--*
--* Permission is hereby granted, free of charge, to any person obtaining
--* a copy of this software and associated documentation files (the
--* "Software"), to deal in the Software without restriction, including
--* without limitation the rights to use, copy, modify, merge, publish,
--* distribute, sublicense, and/or sell copies of the Software, and to
--* permit persons to whom the Software is furnished to do so, subject to
--* the following conditions:
--*
--* The above copyright notice and this permission notice shall be
--* included in all copies or substantial portions of the Software.
--*
--* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
--* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--*****************************************************************************


with Interfaces.C; use Interfaces.C;
with System;

private package Lua.LibInternal is


   LUA_COLIBNAME : aliased constant String := "coroutine" & ASCII.NUL;  --  /usr/include/lualib.h:17

   LUA_TABLIBNAME : aliased constant String := "table" & ASCII.NUL;  --  /usr/include/lualib.h:20

   LUA_IOLIBNAME : aliased constant String := "io" & ASCII.NUL;  --  /usr/include/lualib.h:23

   LUA_OSLIBNAME : aliased constant String := "os" & ASCII.NUL;  --  /usr/include/lualib.h:26

   LUA_STRLIBNAME : aliased constant String := "string" & ASCII.NUL;  --  /usr/include/lualib.h:29

   LUA_UTF8LIBNAME : aliased constant String := "utf8" & ASCII.NUL;  --  /usr/include/lualib.h:32

   LUA_BITLIBNAME : aliased constant String := "bit32" & ASCII.NUL;  --  /usr/include/lualib.h:35

   LUA_MATHLIBNAME : aliased constant String := "math" & ASCII.NUL;  --  /usr/include/lualib.h:38

   LUA_DBLIBNAME : aliased constant String := "debug" & ASCII.NUL;  --  /usr/include/lualib.h:41

   LUA_LOADLIBNAME : aliased constant String := "package" & ASCII.NUL;  --  /usr/include/lualib.h:44
   --  arg-macro: function lua_assert ((void)0
   --    return (void)0;

  --** $Id: lualib.h,v 1.44 2014/02/06 17:32:33 roberto Exp $
  --** Lua standard libraries
  --** See Copyright Notice in lua.h
  --

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
