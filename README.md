# aLua

## Introduction

This is an Ada 2012 project that provides an interface to the
[Lua](http://www.lua.org) language. Lua is a programming language
intended for use in extending and customising host programs (in this
case, those written in Ada). Despite being lightweight in terms of
resource requirements, Lua is powerful enough to support many
programming methodologies including procedural, object-oriented, and
functional approaches. This project currently supports Lua version 5.3.

Both Lua and this Ada interface are free software (MIT-style licence)
and are provided with no guarantees, as set out in the file LICENSE.md.

## Ada - Lua interface design

The main design goal of this project is to provide a full interface to
the Lua library API, but with appropriate changes to ensure that the
resulting API is a slightly safer and more idiomatic Ada than the
original C version. In particular, it should not be necessary for the
user to import anything from the `Interfaces.C` packages.

As far as possible the names of the functions from the C API has been
preserved, but the prefixes of `lua_` or `luaL_` have been removed.
Familiarity with the [C API
documentation](http://www.lua.org/manual/5.3/manual.html#4.8) is
therefore advisable. One notable exception is the `lua_type` function
which has been renamed to `TypeInfo` to avoid a clash with the Ada
keyword.  There are also some additions where appropriate. For example
alongside `IsCFunction` is `IsAdaFunction` which, as the name suggests,
allows Ada and C functions to be distinguished in Ada (although to Lua
code they are equivalent). Most uses of constants in the C API have been
replaced with suitable enumeration types.

The centre of the API is the `Lua_State` type. Values of this type
encapsulate the entirety of the state of a Lua interpreter and a value
has to be passed to almost every other routine as the first parameter.
Internally these are `Limited_Controlled` types so they are
automatically initialised when created and finalised when they go out
of scope.

Communication with Lua code is done via a stack. In general, everything
in Lua (including functions) is a value that can be push onto the stack
and retrieved when required. The generic package `Lua.Userdata` can be
used to push and retrieve Ada tagged types to/from the stack and to
register operations for the type which can be called by Lua code using
the standard syntax `value:op()`. The Ada function can then retrieve
the relevant tagged type value as the top parameter on the stack.

The Lua standard libraries can be added to the global environment of a
particular `Lua_State` using the routines in `Lua.Libs`.

Lua does not support true multi-threading inside Lua code, but it does
support a type of co-operative multi-threading via coroutines (which
are still referred to as threads in the API). These share global state
but each have their own stacks. A `Lua_Thread` created by `NewThread`
is a descendant of `Lua_State` so can be used almost anywhere a
`Lua_State` could be.

The `Resume` function is used to continue executing some code in a
thread until it yields, returns or has an error. The return value of
`Resume` is a `Thread_Status` enumeration value which distinguishes
these cases. Note that the `callk` and similar routines which take a
continuation function are not supported. In the C API they use
`longjmp` to jump out of scope but in Ada this would cause serious
problems for continued execution.

If a reference to a Lua object has to be stored in Ada, a
`Lua_Reference` should be used. This is a reference-counting type so the
Lua library should not garbage-collect the value before the last Ada
reference has been released.

## Example usage

The `examples/` directory contains some examples of usage, covering
basic stack manipulation, using global variables, calling Ada functions
from Lua and vice-versa, saving references to Lua values in Ada for
later reference, using tagged types as Userdata and more.
