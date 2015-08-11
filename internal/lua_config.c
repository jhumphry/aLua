/*
lua_config.c

A small C file to extract certain details from luaconfig.h that explain
how the library was compiled but which are not exported by the library.

Copyright (c) 2015, James Humphry - see LICENSE for terms
*/

#include <lua.h>
#include <luaconf.h>

long lua_conf_multret = LUA_MULTRET;

long lua_conf_luai_maxstack = LUAI_MAXSTACK;
long lua_conf_registry_index = LUA_REGISTRYINDEX;

long long lua_conf_maxinteger = LUA_MAXINTEGER;
long long lua_conf_mininteger = LUA_MININTEGER;
