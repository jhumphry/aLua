-- Lua.Internal

-- Contains the raw interfaces with the Lua library written in C
-- Generated from lua.h and then manually tidied up

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
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;

private package Lua.Internal is


   LUA_VERSION_MAJOR : aliased constant String := "5" & ASCII.NUL;  --  /usr/include/lua.h:19
   LUA_VERSION_MINOR : aliased constant String := "3" & ASCII.NUL;  --  /usr/include/lua.h:20
   LUA_VERSION_NUM : constant := 503;  --  /usr/include/lua.h:21
   LUA_VERSION_RELEASE : aliased constant String := "1" & ASCII.NUL;  --  /usr/include/lua.h:22
   --  unsupported macro: LUA_VERSION "Lua " LUA_VERSION_MAJOR "." LUA_VERSION_MINOR
   --  unsupported macro: LUA_RELEASE LUA_VERSION "." LUA_VERSION_RELEASE
   --  unsupported macro: LUA_COPYRIGHT LUA_RELEASE "  Copyright (C) 1994-2015 Lua.org, PUC-Rio"

   LUA_AUTHORS : aliased constant String := "R. Ierusalimschy, L. H. de Figueiredo, W. Celes" & ASCII.NUL;  --  /usr/include/lua.h:27
   --  unsupported macro: LUA_SIGNATURE "\x1bLua"

   LUA_MULTRET : constant := (-1);  --  /usr/include/lua.h:34
   --  unsupported macro: LUA_REGISTRYINDEX (-LUAI_MAXSTACK - 1000)
   --  arg-macro: function lua_upvalueindex (i)
   --    return LUA_REGISTRYINDEX - (i);

--     LUA_OK : constant := 0;  --  /usr/include/lua.h:47
--     LUA_YIELD : constant := 1;  --  /usr/include/lua.h:48
--     LUA_ERRRUN : constant := 2;  --  /usr/include/lua.h:49
--     LUA_ERRSYNTAX : constant := 3;  --  /usr/include/lua.h:50
--     LUA_ERRMEM : constant := 4;  --  /usr/include/lua.h:51
--     LUA_ERRGCMM : constant := 5;  --  /usr/include/lua.h:52
--     LUA_ERRERR : constant := 6;  --  /usr/include/lua.h:53

--     LUA_TNONE : constant := (-1);  --  /usr/include/lua.h:62
--
--     LUA_TNIL : constant := 0;  --  /usr/include/lua.h:64
--     LUA_TBOOLEAN : constant := 1;  --  /usr/include/lua.h:65
--     LUA_TLIGHTUSERDATA : constant := 2;  --  /usr/include/lua.h:66
--     LUA_TNUMBER : constant := 3;  --  /usr/include/lua.h:67
--     LUA_TSTRING : constant := 4;  --  /usr/include/lua.h:68
--     LUA_TTABLE : constant := 5;  --  /usr/include/lua.h:69
--     LUA_TFUNCTION : constant := 6;  --  /usr/include/lua.h:70
--     LUA_TUSERDATA : constant := 7;  --  /usr/include/lua.h:71
--     LUA_TTHREAD : constant := 8;  --  /usr/include/lua.h:72

   LUA_NUMTAGS : constant := 9;  --  /usr/include/lua.h:74

   LUA_MINSTACK : constant := 20;  --  /usr/include/lua.h:79

   LUA_RIDX_MAINTHREAD : constant := 1;  --  /usr/include/lua.h:83
   LUA_RIDX_GLOBALS : constant := 2;  --  /usr/include/lua.h:84
   --  unsupported macro: LUA_RIDX_LAST LUA_RIDX_GLOBALS

--     LUA_OPADD : constant := 0;  --  /usr/include/lua.h:196
--     LUA_OPSUB : constant := 1;  --  /usr/include/lua.h:197
--     LUA_OPMUL : constant := 2;  --  /usr/include/lua.h:198
--     LUA_OPMOD : constant := 3;  --  /usr/include/lua.h:199
--     LUA_OPPOW : constant := 4;  --  /usr/include/lua.h:200
--     LUA_OPDIV : constant := 5;  --  /usr/include/lua.h:201
--     LUA_OPIDIV : constant := 6;  --  /usr/include/lua.h:202
--     LUA_OPBAND : constant := 7;  --  /usr/include/lua.h:203
--     LUA_OPBOR : constant := 8;  --  /usr/include/lua.h:204
--     LUA_OPBXOR : constant := 9;  --  /usr/include/lua.h:205
--     LUA_OPSHL : constant := 10;  --  /usr/include/lua.h:206
--     LUA_OPSHR : constant := 11;  --  /usr/include/lua.h:207
--     LUA_OPUNM : constant := 12;  --  /usr/include/lua.h:208
--     LUA_OPBNOT : constant := 13;  --  /usr/include/lua.h:209

--     LUA_OPEQ : constant := 0;  --  /usr/include/lua.h:213
--     LUA_OPLT : constant := 1;  --  /usr/include/lua.h:214
--     LUA_OPLE : constant := 2;  --  /usr/include/lua.h:215
   --  arg-macro: procedure lua_call (L, n, r)
   --    lua_callk(L, (n), (r), 0, NULL)
   --  arg-macro: procedure lua_pcall (L, n, r, f)
   --    lua_pcallk(L, (n), (r), (f), 0, NULL)
   --  arg-macro: procedure lua_yield (L, n)
   --    lua_yieldk(L, (n), 0, NULL)

   LUA_GCSTOP : constant := 0;  --  /usr/include/lua.h:302
   LUA_GCRESTART : constant := 1;  --  /usr/include/lua.h:303
   LUA_GCCOLLECT : constant := 2;  --  /usr/include/lua.h:304
   LUA_GCCOUNT : constant := 3;  --  /usr/include/lua.h:305
   LUA_GCCOUNTB : constant := 4;  --  /usr/include/lua.h:306
   LUA_GCSTEP : constant := 5;  --  /usr/include/lua.h:307
   LUA_GCSETPAUSE : constant := 6;  --  /usr/include/lua.h:308
   LUA_GCSETSTEPMUL : constant := 7;  --  /usr/include/lua.h:309
   LUA_GCISRUNNING : constant := 9;  --  /usr/include/lua.h:310
   --  arg-macro: function lua_getextraspace (L)
   --    return (void *)((char *)(L) - LUA_EXTRASPACE);
   --  arg-macro: procedure lua_tonumber (L, i)
   --    lua_tonumberx(L,(i),NULL)
   --  arg-macro: procedure lua_tointeger (L, i)
   --    lua_tointegerx(L,(i),NULL)
   --  arg-macro: procedure lua_pop (L, n)
   --    lua_settop(L, -(n)-1)
   --  arg-macro: procedure lua_newtable (L)
   --    lua_createtable(L, 0, 0)
   --  arg-macro: function lua_register (L, n, f)
   --    return lua_pushcfunction(L, (f)), lua_setglobal(L, (n));
   --  arg-macro: procedure lua_pushcfunction (L, f)
   --    lua_pushcclosure(L, (f), 0)
   --  arg-macro: function lua_isfunction (L, n)
   --    return lua_type(L, (n)) = LUA_TFUNCTION;
   --  arg-macro: function lua_istable (L, n)
   --    return lua_type(L, (n)) = LUA_TTABLE;
   --  arg-macro: function lua_islightuserdata (L, n)
   --    return lua_type(L, (n)) = LUA_TLIGHTUSERDATA;
   --  arg-macro: function lua_isnil (L, n)
   --    return lua_type(L, (n)) = LUA_TNIL;
   --  arg-macro: function lua_isboolean (L, n)
   --    return lua_type(L, (n)) = LUA_TBOOLEAN;
   --  arg-macro: function lua_isthread (L, n)
   --    return lua_type(L, (n)) = LUA_TTHREAD;
   --  arg-macro: function lua_isnone (L, n)
   --    return lua_type(L, (n)) = LUA_TNONE;
   --  arg-macro: function lua_isnoneornil (L, n)
   --    return lua_type(L, (n)) <= 0;
   --  arg-macro: procedure lua_pushliteral (L, s)
   --    lua_pushstring(L, "" s)
   --  arg-macro: procedure lua_pushglobaltable (L)
   --    lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS)
   --  arg-macro: procedure lua_tostring (L, i)
   --    lua_tolstring(L, (i), NULL)
   --  arg-macro: procedure lua_insert (L, idx)
   --    lua_rotate(L, (idx), 1)
   --  arg-macro: function lua_remove (L, idx)
   --    return lua_rotate(L, (idx), -1), lua_pop(L, 1);
   --  arg-macro: function lua_replace (L, idx)
   --    return lua_copy(L, -1, (idx)), lua_pop(L, 1);

   LUA_HOOKCALL : constant := 0;  --  /usr/include/lua.h:402
   LUA_HOOKRET : constant := 1;  --  /usr/include/lua.h:403
   LUA_HOOKLINE : constant := 2;  --  /usr/include/lua.h:404
   LUA_HOOKCOUNT : constant := 3;  --  /usr/include/lua.h:405
   LUA_HOOKTAILCALL : constant := 4;  --  /usr/include/lua.h:406
   --  unsupported macro: LUA_MASKCALL (1 << LUA_HOOKCALL)
   --  unsupported macro: LUA_MASKRET (1 << LUA_HOOKRET)
   --  unsupported macro: LUA_MASKLINE (1 << LUA_HOOKLINE)
   --  unsupported macro: LUA_MASKCOUNT (1 << LUA_HOOKCOUNT)

  --** $Id: lua.h,v 1.328 2015/06/03 13:03:38 roberto Exp $
  --** Lua - A Scripting Language
  --** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
  --** See Copyright Notice at the end of this file
  --

  -- mark for precompiled code ('<esc>Lua')
  -- option for multiple returns in 'lua_pcall' and 'lua_call'
  --** Pseudo-indices
  --** (-LUAI_MAXSTACK is the minimum valid index; we keep some free empty
  --** space after that to help overflow detection)
  --

  -- thread status
   --  skipped empty struct lua_State

  --** basic types
  --

  -- minimum Lua stack available to a C function
  -- predefined values in the registry
  -- type of numbers in Lua
   subtype lua_Number is double;  -- /usr/include/lua.h:89

  -- type for integer functions
   subtype lua_Integer is Long_Long_Integer;  -- /usr/include/lua.h:93

  -- unsigned integer type
   subtype lua_Unsigned is Extensions.unsigned_long_long;  -- /usr/include/lua.h:96

  -- type for continuation-function contexts
   subtype lua_KContext is Interfaces.C.ptrdiff_t;  -- /usr/include/lua.h:99

  --** Type for C functions registered with Lua
  --

   type lua_CFunction is access function (arg1 : System.Address) return int;
   pragma Convention (C, lua_CFunction);  -- /usr/include/lua.h:105

  --** Type for continuation functions
  --

   type lua_KFunction is access function
        (arg1 : System.Address;
         arg2 : int;
         arg3 : lua_KContext) return int;
   pragma Convention (C, lua_KFunction);  -- /usr/include/lua.h:110

  --** Type for functions that read/write blocks when loading/dumping Lua chunks
  --

   type lua_Reader is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : access Interfaces.C.size_t) return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, lua_Reader);  -- /usr/include/lua.h:116

   type lua_Writer is access function
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : Interfaces.C.size_t;
         arg4 : System.Address) return int;
   pragma Convention (C, lua_Writer);  -- /usr/include/lua.h:118

  --** Type for memory-allocation functions
  --

   type lua_Alloc is access function
        (ud : System.Address;
         ptr : System.Address;
         osize : Interfaces.C.size_t;
         nsize : Interfaces.C.size_t) return System.Address;
   pragma Convention (C, lua_Alloc);  -- /usr/include/lua.h:124

  --** generic extra include file
  --

  --** RCS ident string
  --

   lua_ident : aliased Interfaces.C.char_array (size_t);  -- /usr/include/lua.h:139
   pragma Import (C, lua_ident, "lua_ident");

  --** state manipulation
  --

   function lua_newstate (arg1 : lua_Alloc; arg2 : System.Address) return System.Address;  -- /usr/include/lua.h:145
   pragma Import (C, lua_newstate, "lua_newstate");

   procedure lua_close (arg1 : System.Address);  -- /usr/include/lua.h:146
   pragma Import (C, lua_close, "lua_close");

   function lua_newthread (arg1 : System.Address) return System.Address;  -- /usr/include/lua.h:147
   pragma Import (C, lua_newthread, "lua_newthread");

   function lua_atpanic (arg1 : System.Address; arg2 : lua_CFunction) return lua_CFunction;  -- /usr/include/lua.h:149
   pragma Import (C, lua_atpanic, "lua_atpanic");

   function lua_version (arg1 : System.Address) return access constant lua_Number;  -- /usr/include/lua.h:152
   pragma Import (C, lua_version, "lua_version");

  --** basic stack manipulation
  --

   function lua_absindex (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:158
   pragma Import (C, lua_absindex, "lua_absindex");

   function lua_gettop (arg1 : System.Address) return int;  -- /usr/include/lua.h:159
   pragma Import (C, lua_gettop, "lua_gettop");

   procedure lua_settop (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:160
   pragma Import (C, lua_settop, "lua_settop");

   procedure lua_pushvalue (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:161
   pragma Import (C, lua_pushvalue, "lua_pushvalue");

   procedure lua_rotate
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int);  -- /usr/include/lua.h:162
   pragma Import (C, lua_rotate, "lua_rotate");

   procedure lua_copy
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int);  -- /usr/include/lua.h:163
   pragma Import (C, lua_copy, "lua_copy");

   function lua_checkstack (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:164
   pragma Import (C, lua_checkstack, "lua_checkstack");

   procedure lua_xmove
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int);  -- /usr/include/lua.h:166
   pragma Import (C, lua_xmove, "lua_xmove");

  --** access functions (stack -> C)
  --

   function lua_isnumber (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:173
   pragma Import (C, lua_isnumber, "lua_isnumber");

   function lua_isstring (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:174
   pragma Import (C, lua_isstring, "lua_isstring");

   function lua_iscfunction (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:175
   pragma Import (C, lua_iscfunction, "lua_iscfunction");

   function lua_isinteger (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:176
   pragma Import (C, lua_isinteger, "lua_isinteger");

   function lua_isuserdata (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:177
   pragma Import (C, lua_isuserdata, "lua_isuserdata");

   function lua_type (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:178
   pragma Import (C, lua_type, "lua_type");

   function lua_typename (arg1 : System.Address; arg2 : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:179
   pragma Import (C, lua_typename, "lua_typename");

   function lua_tonumberx
     (arg1 : System.Address;
      arg2 : int;
      arg3 : access int) return lua_Number;  -- /usr/include/lua.h:181
   pragma Import (C, lua_tonumberx, "lua_tonumberx");

   function lua_tointegerx
     (arg1 : System.Address;
      arg2 : int;
      arg3 : access int) return lua_Integer;  -- /usr/include/lua.h:182
   pragma Import (C, lua_tointegerx, "lua_tointegerx");

   function lua_toboolean (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:183
   pragma Import (C, lua_toboolean, "lua_toboolean");

   function lua_tolstring
     (arg1 : System.Address;
      arg2 : int;
      arg3 : access Interfaces.C.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:184
   pragma Import (C, lua_tolstring, "lua_tolstring");

   function lua_rawlen (arg1 : System.Address; arg2 : int) return Interfaces.C.size_t;  -- /usr/include/lua.h:185
   pragma Import (C, lua_rawlen, "lua_rawlen");

   function lua_tocfunction (arg1 : System.Address; arg2 : int) return lua_CFunction;  -- /usr/include/lua.h:186
   pragma Import (C, lua_tocfunction, "lua_tocfunction");

   function lua_touserdata (arg1 : System.Address; arg2 : int) return System.Address;  -- /usr/include/lua.h:187
   pragma Import (C, lua_touserdata, "lua_touserdata");

   function lua_tothread (arg1 : System.Address; arg2 : int) return System.Address;  -- /usr/include/lua.h:188
   pragma Import (C, lua_tothread, "lua_tothread");

   function lua_topointer (arg1 : System.Address; arg2 : int) return System.Address;  -- /usr/include/lua.h:189
   pragma Import (C, lua_topointer, "lua_topointer");

  --** Comparison and arithmetic functions
  --

   procedure lua_arith (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:211
   pragma Import (C, lua_arith, "lua_arith");

   function lua_rawequal
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int) return int;  -- /usr/include/lua.h:217
   pragma Import (C, lua_rawequal, "lua_rawequal");

   function lua_compare
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int;
      arg4 : int) return int;  -- /usr/include/lua.h:218
   pragma Import (C, lua_compare, "lua_compare");

  --** push functions (C -> stack)
  --

   procedure lua_pushnil (arg1 : System.Address);  -- /usr/include/lua.h:224
   pragma Import (C, lua_pushnil, "lua_pushnil");

   procedure lua_pushnumber (arg1 : System.Address; arg2 : lua_Number);  -- /usr/include/lua.h:225
   pragma Import (C, lua_pushnumber, "lua_pushnumber");

   procedure lua_pushinteger (arg1 : System.Address; arg2 : lua_Integer);  -- /usr/include/lua.h:226
   pragma Import (C, lua_pushinteger, "lua_pushinteger");

   function lua_pushlstring
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:227
   pragma Import (C, lua_pushlstring, "lua_pushlstring");

   function lua_pushstring (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:228
   pragma Import (C, lua_pushstring, "lua_pushstring");

   function lua_pushvfstring
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : access System.Address) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:229
   pragma Import (C, lua_pushvfstring, "lua_pushvfstring");

   function lua_pushfstring (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:231
   pragma Import (C, lua_pushfstring, "lua_pushfstring");

   procedure lua_pushcclosure
     (arg1 : System.Address;
      arg2 : lua_CFunction;
      arg3 : int);  -- /usr/include/lua.h:232
   pragma Import (C, lua_pushcclosure, "lua_pushcclosure");

   procedure lua_pushboolean (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:233
   pragma Import (C, lua_pushboolean, "lua_pushboolean");

   procedure lua_pushlightuserdata (arg1 : System.Address; arg2 : System.Address);  -- /usr/include/lua.h:234
   pragma Import (C, lua_pushlightuserdata, "lua_pushlightuserdata");

   function lua_pushthread (arg1 : System.Address) return int;  -- /usr/include/lua.h:235
   pragma Import (C, lua_pushthread, "lua_pushthread");

  --** get functions (Lua -> stack)
  --

   function lua_getglobal (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lua.h:241
   pragma Import (C, lua_getglobal, "lua_getglobal");

   function lua_gettable (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:242
   pragma Import (C, lua_gettable, "lua_gettable");

   function lua_getfield
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lua.h:243
   pragma Import (C, lua_getfield, "lua_getfield");

   function lua_geti
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_Integer) return int;  -- /usr/include/lua.h:244
   pragma Import (C, lua_geti, "lua_geti");

   function lua_rawget (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:245
   pragma Import (C, lua_rawget, "lua_rawget");

   function lua_rawgeti
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_Integer) return int;  -- /usr/include/lua.h:246
   pragma Import (C, lua_rawgeti, "lua_rawgeti");

   function lua_rawgetp
     (arg1 : System.Address;
      arg2 : int;
      arg3 : System.Address) return int;  -- /usr/include/lua.h:247
   pragma Import (C, lua_rawgetp, "lua_rawgetp");

   procedure lua_createtable
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int);  -- /usr/include/lua.h:249
   pragma Import (C, lua_createtable, "lua_createtable");

   function lua_newuserdata (arg1 : System.Address; arg2 : Interfaces.C.size_t) return System.Address;  -- /usr/include/lua.h:250
   pragma Import (C, lua_newuserdata, "lua_newuserdata");

   function lua_getmetatable (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:251
   pragma Import (C, lua_getmetatable, "lua_getmetatable");

   function lua_getuservalue (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:252
   pragma Import (C, lua_getuservalue, "lua_getuservalue");

  --** set functions (stack -> Lua)
  --

   procedure lua_setglobal (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/lua.h:258
   pragma Import (C, lua_setglobal, "lua_setglobal");

   procedure lua_settable (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:259
   pragma Import (C, lua_settable, "lua_settable");

   procedure lua_setfield
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/lua.h:260
   pragma Import (C, lua_setfield, "lua_setfield");

   procedure lua_seti
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_Integer);  -- /usr/include/lua.h:261
   pragma Import (C, lua_seti, "lua_seti");

   procedure lua_rawset (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:262
   pragma Import (C, lua_rawset, "lua_rawset");

   procedure lua_rawseti
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_Integer);  -- /usr/include/lua.h:263
   pragma Import (C, lua_rawseti, "lua_rawseti");

   procedure lua_rawsetp
     (arg1 : System.Address;
      arg2 : int;
      arg3 : System.Address);  -- /usr/include/lua.h:264
   pragma Import (C, lua_rawsetp, "lua_rawsetp");

   function lua_setmetatable (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:265
   pragma Import (C, lua_setmetatable, "lua_setmetatable");

   procedure lua_setuservalue (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:266
   pragma Import (C, lua_setuservalue, "lua_setuservalue");

  --** 'load' and 'call' functions (load and run Lua code)
  --

   procedure lua_callk
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int;
      arg4 : lua_KContext;
      arg5 : lua_KFunction);  -- /usr/include/lua.h:272
   pragma Import (C, lua_callk, "lua_callk");

   function lua_pcallk
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int;
      arg4 : int;
      arg5 : lua_KContext;
      arg6 : lua_KFunction) return int;  -- /usr/include/lua.h:276
   pragma Import (C, lua_pcallk, "lua_pcallk");

   function lua_load
     (arg1 : System.Address;
      arg2 : lua_Reader;
      arg3 : System.Address;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lua.h:280
   pragma Import (C, lua_load, "lua_load");

   function lua_dump
     (arg1 : System.Address;
      arg2 : lua_Writer;
      arg3 : System.Address;
      arg4 : int) return int;  -- /usr/include/lua.h:283
   pragma Import (C, lua_dump, "lua_dump");

  --** coroutine functions
  --

   function lua_yieldk
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_KContext;
      arg4 : lua_KFunction) return int;  -- /usr/include/lua.h:289
   pragma Import (C, lua_yieldk, "lua_yieldk");

   function lua_resume
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int) return int;  -- /usr/include/lua.h:291
   pragma Import (C, lua_resume, "lua_resume");

   function lua_status (arg1 : System.Address) return int;  -- /usr/include/lua.h:292
   pragma Import (C, lua_status, "lua_status");

   function lua_isyieldable (arg1 : System.Address) return int;  -- /usr/include/lua.h:293
   pragma Import (C, lua_isyieldable, "lua_isyieldable");

  --** garbage-collection function and options
  --

   function lua_gc
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int) return int;  -- /usr/include/lua.h:312
   pragma Import (C, lua_gc, "lua_gc");

  --** miscellaneous functions
  --

   function lua_error (arg1 : System.Address) return int;  -- /usr/include/lua.h:319
   pragma Import (C, lua_error, "lua_error");

   function lua_next (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lua.h:321
   pragma Import (C, lua_next, "lua_next");

   procedure lua_concat (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:323
   pragma Import (C, lua_concat, "lua_concat");

   procedure lua_len (arg1 : System.Address; arg2 : int);  -- /usr/include/lua.h:324
   pragma Import (C, lua_len, "lua_len");

   function lua_stringtonumber (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.size_t;  -- /usr/include/lua.h:326
   pragma Import (C, lua_stringtonumber, "lua_stringtonumber");

   function lua_getallocf (arg1 : System.Address; arg2 : System.Address) return lua_Alloc;  -- /usr/include/lua.h:328
   pragma Import (C, lua_getallocf, "lua_getallocf");

   procedure lua_setallocf
     (arg1 : System.Address;
      arg2 : lua_Alloc;
      arg3 : System.Address);  -- /usr/include/lua.h:329
   pragma Import (C, lua_setallocf, "lua_setallocf");

  --** {==============================================================
  --** some useful macros
  --** ===============================================================
  --

  -- }==============================================================
  --** {==============================================================
  --** compatibility macros for unsigned conversions
  --** ===============================================================
  --

  -- }==============================================================
  --** {======================================================================
  --** Debug API
  --** =======================================================================
  --

  --** Event codes
  --

  --** Event masks
  --

  -- activation record
  -- Functions to be called by the debugger in specific events
   type lua_Hook is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, lua_Hook);  -- /usr/include/lua.h:421

   function lua_getstack
     (arg1 : System.Address;
      arg2 : int;
      arg3 : System.Address) return int;  -- /usr/include/lua.h:424
   pragma Import (C, lua_getstack, "lua_getstack");

   function lua_getinfo
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : System.Address) return int;  -- /usr/include/lua.h:425
   pragma Import (C, lua_getinfo, "lua_getinfo");

   function lua_getlocal
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:426
   pragma Import (C, lua_getlocal, "lua_getlocal");

   function lua_setlocal
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:427
   pragma Import (C, lua_setlocal, "lua_setlocal");

   function lua_getupvalue
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:428
   pragma Import (C, lua_getupvalue, "lua_getupvalue");

   function lua_setupvalue
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:429
   pragma Import (C, lua_setupvalue, "lua_setupvalue");

   function lua_upvalueid
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int) return System.Address;  -- /usr/include/lua.h:431
   pragma Import (C, lua_upvalueid, "lua_upvalueid");

   procedure lua_upvaluejoin
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int;
      arg4 : int;
      arg5 : int);  -- /usr/include/lua.h:432
   pragma Import (C, lua_upvaluejoin, "lua_upvaluejoin");

   procedure lua_sethook
     (arg1 : System.Address;
      arg2 : lua_Hook;
      arg3 : int;
      arg4 : int);  -- /usr/include/lua.h:435
   pragma Import (C, lua_sethook, "lua_sethook");

   function lua_gethook (arg1 : System.Address) return lua_Hook;  -- /usr/include/lua.h:436
   pragma Import (C, lua_gethook, "lua_gethook");

   function lua_gethookmask (arg1 : System.Address) return int;  -- /usr/include/lua.h:437
   pragma Import (C, lua_gethookmask, "lua_gethookmask");

   function lua_gethookcount (arg1 : System.Address) return int;  -- /usr/include/lua.h:438
   pragma Import (C, lua_gethookcount, "lua_gethookcount");

  -- (n)
  -- (n) 'global', 'local', 'field', 'method'
  -- (S) 'Lua', 'C', 'main', 'tail'
  -- (S)
  -- (l)
  -- (S)
  -- (S)
  -- (u) number of upvalues
  -- (u) number of parameters
  -- (u)
  -- (t)
  -- (S)
  -- private part
  -- active function
   --  skipped empty struct CallInfo

   subtype anon1072_anon1110_array is Interfaces.C.char_array (0 .. 59);
   type lua_Debug is record
      event : aliased int;  -- /usr/include/lua.h:442
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:443
      namewhat : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:444
      what : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:445
      source : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lua.h:446
      currentline : aliased int;  -- /usr/include/lua.h:447
      linedefined : aliased int;  -- /usr/include/lua.h:448
      lastlinedefined : aliased int;  -- /usr/include/lua.h:449
      nups : aliased unsigned_char;  -- /usr/include/lua.h:450
      nparams : aliased unsigned_char;  -- /usr/include/lua.h:451
      isvararg : aliased char;  -- /usr/include/lua.h:452
      istailcall : aliased char;  -- /usr/include/lua.h:453
      short_src : aliased anon1072_anon1110_array;  -- /usr/include/lua.h:454
      i_ci : System.Address;  -- /usr/include/lua.h:456
   end record;
   pragma Convention (C_Pass_By_Copy, lua_Debug);  -- /usr/include/lua.h:441

  -- }======================================================================


end Lua.Internal;
