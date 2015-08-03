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
and are provided with no guarantees, as set out in the file LICENSE.

## Usage notes

### Ada - Lua interface design

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

### `Lua_State` and coroutines

The centre of the API is the `Lua_State` type. Values of this type
encapsulate the entirety of the state of a Lua interpreter and a value
has to be passed to almost every other routine as the first parameter.
Internally these are `Limited_Controlled` types so they are
automatically initialised when created and finalised when they go out
of scope.

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

### Communicating values between Lua and Ada

Communication with Lua code is done via a stack. In general, everything
in Lua (including functions) is a value that can be push onto the stack
and retrieved when required. There are many functions in the package
Lua for manipulating the stack. Pay particular attention to the fact
that some routines such as `ToString` and `ToNumber` can do silent
conversion of the values on the stack if they are compatible - this
side-effect can be a trap if you are not careful.

The generic package `Lua.Userdata` can be used to push and retrieve Ada
tagged types or type classes to/from the stack, and to register
operations for the type which can then be called by Lua code using the
standard syntax `value:op()`. The Ada function can then retrieve the
relevant tagged type value as the first parameter - the top of the
stack. Alternatively Ada functions registered as global functions in
Lua can take parameters of the new userdata type.

The `Push`, `Push_Class`, `ToUserdata`, `ToUserdata_Class` and
`IsAdaUserdata` routines allow for compatible conversions between
userdata types. Suppose a tagged type P has a derived type C. If
`Lua.Userdata` is instantiated for the type P then a new P-userdata
type is created in Lua. Access to P objects can be pushed as direct or
class-wide access values to create P-userdata values on the stack. C
objects can only be pushed as P'class-wide access values. From Lua, the
P-userdata created from C objects have exactly the same metatable as
the P-userdata created from P objects so `C:op()` will call the same
Ada routine as `P:op()`. When `ToUserdata` and `ToUserdata_Class` are
invoked on P-userdata created from C objects, the conversion to an
access-to-P or access-to-P'Class will always succeed as type C is
covered by type P.

Now consider what happens if `Lua.Userdata` is instantiated for type C
to create a C-userdata type in Lua. First it is important to realise
that there is no automatic inheritance of the metatable from P-userdata
types so all the routines will need to be explicitly re-registered for
C-userdata (if they are not to be overridden). However any
re-registered routines will work without modification as all the
C-userdata are convertible into P-userdata. Additional routines that
only expect C-userdata can be written in Ada and registered with Lua
against the C-userdata metatable only.

However a routine that expects to work with C-userdata will not work
with a P-userdata that happens to have been created from a C object
which was pushed via an access-to-P'Class value. The conversion from
access-to-P'Class to access-to-C or access-to-C'Class would be valid at
run-time from an Ada point of view, but it would not make any sense
from a Lua point of view. The difference between P-userdata values that
can be passed to such a routine (based on objects that are in both
C'Class and P'Class) and values that cannot (based on objects in
P'Class but not C'Class) is not visible at all in Lua and so any use of
the distinction would likely to lead to error-strewn code. It is
therefore not supported.

In practice it is probably best to avoid instantiating multiple types
of Lua userdata for every point in an Ada tagged type hierarchy. Often
it will be appropriate just to instantiate a userdata type for a tagged
type (possibly abstract) towards the top of the hierarchy and to
conceal the further fine distinctions from the code written in Lua.

### Keeping references to Lua objects in Ada

If a reference to a Lua object has to be stored in Ada, a
`Lua_Reference` should be used. This is a reference-counting type so the
Lua interpreter should not garbage-collect the value before the last Ada
reference has been released.

## Example usage

The `examples/` directory contains some examples of usage, covering
basic stack manipulation, using global variables, calling Ada functions
from Lua and vice-versa, saving references to Lua values in Ada for
later reference, using tagged types as Userdata and more.
