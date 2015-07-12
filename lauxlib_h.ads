pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with lua_h;
with System;
with stddef_h;
limited with stdio_h;

package lauxlib_h is

   --  unsupported macro: LUA_ERRFILE (LUA_ERRERR+1)
   --  unsupported macro: LUAL_NUMSIZES (sizeof(lua_Integer)*16 + sizeof(lua_Number))
   --  arg-macro: procedure luaL_checkversion (L)
   --    luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES)

   LUA_NOREF : constant := (-2);  --  /usr/include/lauxlib.h:69
   LUA_REFNIL : constant := (-1);  --  /usr/include/lauxlib.h:70
   --  arg-macro: procedure luaL_loadfile (L, f)
   --    luaL_loadfilex(L,f,NULL)
   --  arg-macro: procedure luaL_newlibtable (L, l)
   --    lua_createtable(L, 0, sizeof(l)/sizeof((l)(0)) - 1)
   --  arg-macro: function luaL_newlib (L, l)
   --    return luaL_checkversion(L), luaL_newlibtable(L,l), luaL_setfuncs(L,l,0);
   --  arg-macro: function luaL_argcheck (L, cond, arg, e((void)((cond)  or else  luaL_argerror(L, (arg), (extramsg)))
   --    return (void)((cond)  or else  luaL_argerror(L, (arg), (extramsg)));
   --  arg-macro: function luaL_checkstring (L, n)
   --    return luaL_checklstring(L, (n), NULL);
   --  arg-macro: function luaL_optstring (L, n, d)
   --    return luaL_optlstring(L, (n), (d), NULL);
   --  arg-macro: procedure luaL_typename (L, i)
   --    lua_typename(L, lua_type(L,(i)))
   --  arg-macro: function luaL_dofile (L, fn)
   --    return luaL_loadfile(L, fn)  or else  lua_pcall(L, 0, LUA_MULTRET, 0);
   --  arg-macro: function luaL_dostring (L, s)
   --    return luaL_loadstring(L, s)  or else  lua_pcall(L, 0, LUA_MULTRET, 0);
   --  arg-macro: function luaL_getmetatable (L, n)
   --    return lua_getfield(L, LUA_REGISTRYINDEX, (n));
   --  arg-macro: function luaL_opt (L, f, n, d)
   --    return lua_isnoneornil(L,(n)) ? (d) : f(L,(n));
   --  arg-macro: procedure luaL_loadbuffer (L, s, sz, n)
   --    luaL_loadbufferx(L,s,sz,n,NULL)
   --  arg-macro: function luaL_addchar (B, c)
   --    return (void)((B).n < (B).size  or else  luaL_prepbuffsize((B), 1)), ((B).b((B).n++) := (c));
   --  arg-macro: function luaL_addsize (B, s)
   --    return (B).n += (s);
   --  arg-macro: procedure luaL_prepbuffer (B)
   --    luaL_prepbuffsize(B, LUAL_BUFFERSIZE)

   LUA_FILEHANDLE : aliased constant String := "FILE*" & ASCII.NUL;  --  /usr/include/lauxlib.h:182
   --  arg-macro: procedure lua_writestring (s, l)
   --    fwrite((s), sizeof(char), (l), stdout)
   --  arg-macro: function lua_writeline ()
   --    return lua_writestring("" & ASCII.LF & "", 1), fflush(stdout);
   --  arg-macro: function lua_writestringerror (s, p)
   --    return fprintf(stderr, (s), (p)), fflush(stderr);

  --** $Id: lauxlib.h,v 1.128 2014/10/29 16:11:17 roberto Exp $
  --** Auxiliary functions for building Lua libraries
  --** See Copyright Notice in lua.h
  -- 

  -- extra error code for 'luaL_load'  
   type luaL_Reg is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:24
      func : lua_h.lua_CFunction;  -- /usr/include/lauxlib.h:25
   end record;
   pragma Convention (C_Pass_By_Copy, luaL_Reg);  -- /usr/include/lauxlib.h:23

   procedure luaL_checkversion_u
     (arg1 : System.Address;
      arg2 : lua_h.lua_Number;
      arg3 : stddef_h.size_t);  -- /usr/include/lauxlib.h:31
   pragma Import (C, luaL_checkversion_u, "luaL_checkversion_");

   function luaL_getmetafield
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:35
   pragma Import (C, luaL_getmetafield, "luaL_getmetafield");

   function luaL_callmeta
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:36
   pragma Import (C, luaL_callmeta, "luaL_callmeta");

   function luaL_tolstring
     (arg1 : System.Address;
      arg2 : int;
      arg3 : access stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:37
   pragma Import (C, luaL_tolstring, "luaL_tolstring");

   function luaL_argerror
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:38
   pragma Import (C, luaL_argerror, "luaL_argerror");

   function luaL_checklstring
     (arg1 : System.Address;
      arg2 : int;
      arg3 : access stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:39
   pragma Import (C, luaL_checklstring, "luaL_checklstring");

   function luaL_optlstring
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : access stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:41
   pragma Import (C, luaL_optlstring, "luaL_optlstring");

   function luaL_checknumber (arg1 : System.Address; arg2 : int) return lua_h.lua_Number;  -- /usr/include/lauxlib.h:43
   pragma Import (C, luaL_checknumber, "luaL_checknumber");

   function luaL_optnumber
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_h.lua_Number) return lua_h.lua_Number;  -- /usr/include/lauxlib.h:44
   pragma Import (C, luaL_optnumber, "luaL_optnumber");

   function luaL_checkinteger (arg1 : System.Address; arg2 : int) return lua_h.lua_Integer;  -- /usr/include/lauxlib.h:46
   pragma Import (C, luaL_checkinteger, "luaL_checkinteger");

   function luaL_optinteger
     (arg1 : System.Address;
      arg2 : int;
      arg3 : lua_h.lua_Integer) return lua_h.lua_Integer;  -- /usr/include/lauxlib.h:47
   pragma Import (C, luaL_optinteger, "luaL_optinteger");

   procedure luaL_checkstack
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/lauxlib.h:50
   pragma Import (C, luaL_checkstack, "luaL_checkstack");

   procedure luaL_checktype
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int);  -- /usr/include/lauxlib.h:51
   pragma Import (C, luaL_checktype, "luaL_checktype");

   procedure luaL_checkany (arg1 : System.Address; arg2 : int);  -- /usr/include/lauxlib.h:52
   pragma Import (C, luaL_checkany, "luaL_checkany");

   function luaL_newmetatable (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:54
   pragma Import (C, luaL_newmetatable, "luaL_newmetatable");

   procedure luaL_setmetatable (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/lauxlib.h:55
   pragma Import (C, luaL_setmetatable, "luaL_setmetatable");

   function luaL_testudata
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;  -- /usr/include/lauxlib.h:56
   pragma Import (C, luaL_testudata, "luaL_testudata");

   function luaL_checkudata
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;  -- /usr/include/lauxlib.h:57
   pragma Import (C, luaL_checkudata, "luaL_checkudata");

   procedure luaL_where (arg1 : System.Address; arg2 : int);  -- /usr/include/lauxlib.h:59
   pragma Import (C, luaL_where, "luaL_where");

   function luaL_error (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/lauxlib.h:60
   pragma Import (C, luaL_error, "luaL_error");

   function luaL_checkoption
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : System.Address) return int;  -- /usr/include/lauxlib.h:62
   pragma Import (C, luaL_checkoption, "luaL_checkoption");

   function luaL_fileresult
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:65
   pragma Import (C, luaL_fileresult, "luaL_fileresult");

   function luaL_execresult (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lauxlib.h:66
   pragma Import (C, luaL_execresult, "luaL_execresult");

  -- pre-defined references  
   function luaL_ref (arg1 : System.Address; arg2 : int) return int;  -- /usr/include/lauxlib.h:72
   pragma Import (C, luaL_ref, "luaL_ref");

   procedure luaL_unref
     (arg1 : System.Address;
      arg2 : int;
      arg3 : int);  -- /usr/include/lauxlib.h:73
   pragma Import (C, luaL_unref, "luaL_unref");

   function luaL_loadfilex
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:75
   pragma Import (C, luaL_loadfilex, "luaL_loadfilex");

   function luaL_loadbufferx
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : stddef_h.size_t;
      arg4 : Interfaces.C.Strings.chars_ptr;
      arg5 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:80
   pragma Import (C, luaL_loadbufferx, "luaL_loadbufferx");

   function luaL_loadstring (arg1 : System.Address; arg2 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:82
   pragma Import (C, luaL_loadstring, "luaL_loadstring");

   function luaL_newstate return System.Address;  -- /usr/include/lauxlib.h:84
   pragma Import (C, luaL_newstate, "luaL_newstate");

   function luaL_len (arg1 : System.Address; arg2 : int) return lua_h.lua_Integer;  -- /usr/include/lauxlib.h:86
   pragma Import (C, luaL_len, "luaL_len");

   function luaL_gsub
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:88
   pragma Import (C, luaL_gsub, "luaL_gsub");

   procedure luaL_setfuncs
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : int);  -- /usr/include/lauxlib.h:91
   pragma Import (C, luaL_setfuncs, "luaL_setfuncs");

   function luaL_getsubtable
     (arg1 : System.Address;
      arg2 : int;
      arg3 : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/lauxlib.h:93
   pragma Import (C, luaL_getsubtable, "luaL_getsubtable");

   procedure luaL_traceback
     (arg1 : System.Address;
      arg2 : System.Address;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : int);  -- /usr/include/lauxlib.h:95
   pragma Import (C, luaL_traceback, "luaL_traceback");

   procedure luaL_requiref
     (arg1 : System.Address;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : lua_h.lua_CFunction;
      arg4 : int);  -- /usr/include/lauxlib.h:98
   pragma Import (C, luaL_requiref, "luaL_requiref");

  --** ===============================================================
  --** some useful macros
  --** ===============================================================
  -- 

  --** {======================================================
  --** Generic Buffer manipulation
  --** =======================================================
  -- 

  -- buffer address  
   subtype anon1660_anon1662_array is Interfaces.C.char_array (0 .. 8191);
   type luaL_Buffer is record
      b : Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:141
      size : aliased stddef_h.size_t;  -- /usr/include/lauxlib.h:142
      n : aliased stddef_h.size_t;  -- /usr/include/lauxlib.h:143
      L : System.Address;  -- /usr/include/lauxlib.h:144
      initb : aliased anon1660_anon1662_array;  -- /usr/include/lauxlib.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, luaL_Buffer);  -- /usr/include/lauxlib.h:140

  -- buffer size  
  -- number of characters in buffer  
  -- initial buffer  
   procedure luaL_buffinit (arg1 : System.Address; arg2 : access luaL_Buffer);  -- /usr/include/lauxlib.h:155
   pragma Import (C, luaL_buffinit, "luaL_buffinit");

   function luaL_prepbuffsize (arg1 : access luaL_Buffer; arg2 : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:156
   pragma Import (C, luaL_prepbuffsize, "luaL_prepbuffsize");

   procedure luaL_addlstring
     (arg1 : access luaL_Buffer;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : stddef_h.size_t);  -- /usr/include/lauxlib.h:157
   pragma Import (C, luaL_addlstring, "luaL_addlstring");

   procedure luaL_addstring (arg1 : access luaL_Buffer; arg2 : Interfaces.C.Strings.chars_ptr);  -- /usr/include/lauxlib.h:158
   pragma Import (C, luaL_addstring, "luaL_addstring");

   procedure luaL_addvalue (arg1 : access luaL_Buffer);  -- /usr/include/lauxlib.h:159
   pragma Import (C, luaL_addvalue, "luaL_addvalue");

   procedure luaL_pushresult (arg1 : access luaL_Buffer);  -- /usr/include/lauxlib.h:160
   pragma Import (C, luaL_pushresult, "luaL_pushresult");

   procedure luaL_pushresultsize (arg1 : access luaL_Buffer; arg2 : stddef_h.size_t);  -- /usr/include/lauxlib.h:161
   pragma Import (C, luaL_pushresultsize, "luaL_pushresultsize");

   function luaL_buffinitsize
     (arg1 : System.Address;
      arg2 : access luaL_Buffer;
      arg3 : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/lauxlib.h:162
   pragma Import (C, luaL_buffinitsize, "luaL_buffinitsize");

  -- }======================================================  
  --** {======================================================
  --** File handles for IO library
  --** =======================================================
  -- 

  --** A file handle is a userdata with metatable 'LUA_FILEHANDLE' and
  --** initial structure 'luaL_Stream' (it may contain other fields
  --** after that initial structure).
  -- 

  -- stream (NULL for incompletely created streams)  
   type luaL_Stream is record
      f : access stdio_h.FILE;  -- /usr/include/lauxlib.h:186
      closef : lua_h.lua_CFunction;  -- /usr/include/lauxlib.h:187
   end record;
   pragma Convention (C_Pass_By_Copy, luaL_Stream);  -- /usr/include/lauxlib.h:185

  -- to close stream (NULL for closed streams)  
  -- }======================================================  
  -- compatibility with old module system  
  --** {==================================================================
  --** "Abstraction Layer" for basic report of messages and errors
  --** ===================================================================
  -- 

  -- print a string  
  -- print a newline and flush the output  
  -- print an error message  
  -- }==================================================================  
  --** {============================================================
  --** Compatibility with deprecated conversions
  --** =============================================================
  -- 

  -- }============================================================  
end lauxlib_h;
