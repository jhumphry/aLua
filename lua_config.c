/*
lua_config.c

A small C file to extract certain details from luaconfig.h that explain
how the library was compiled but which are not exported by the library

Copyright (c) 2015, James Humphry
 */

#include <luaconf.h>

long lua_conf_luai_maxstack = LUAI_MAXSTACK;
