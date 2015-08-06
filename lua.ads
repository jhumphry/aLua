-- Lua
-- an Ada 2012 interface to Lua

-- Copyright (c) 2015, James Humphry

-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Finalization;

private with System;
private with Interfaces.C;

package Lua is

   -- Types used by Lua

   subtype Lua_Number is Long_Float;
   subtype Lua_Integer is Long_Long_Integer;

   -- Enumerations

   type Thread_Status is (OK, YIELD, ERRRUN, ERRSYNTAX,
                          ERRMEM, ERRGCMM, ERRERR, ERRFILE);

   type Arith_Op is (OPADD, OPSUB, OPMUL, OPMOD, OPPOW, OPDIV, OPIDIV, OPBAND,
                     OPBOR, OPBXOR, OPSHL, OPSHR, OPUNM, OPBNOT);

   type Comparison_Op is (OPEQ, OPLT, OPLE);

   type GC_Inputs is (GCSTOP, GCRESTART, GCCOLLECT, GCCOUNT,
                      GCCOUNTB, GCSTEP, GCSETPAUSE, GCSETSTEPMUL, GCISRUNNING);
   for GC_Inputs use (GCSTOP => 0, GCRESTART => 1, GCCOLLECT => 2,
                      GCCOUNT => 3, GCCOUNTB => 4, GCSTEP => 5,
                      GCSETPAUSE => 6, GCSETSTEPMUL => 7, GCISRUNNING => 9);

   subtype GC_Op is GC_Inputs range GCSTOP..GCSTEP;
   subtype GC_Param is GC_Inputs range GCSETPAUSE..GCSETSTEPMUL;
   subtype GC_Queries is GC_Inputs range GCISRUNNING..GCISRUNNING;

   type Lua_Type is (TNONE, TNIL, TBOOLEAN, TLIGHTUSERDATA, TNUMBER, TSTRING,
                     TTABLE, TFUNCTION, TUSERDATA, TTHREAD, TNUMTAGS);

   type Lua_ChunkMode is (Binary, Text, Binary_and_Text);

   -- Exceptions

   -- Lua_Error is raised whenever there is a violation of a constraint
   -- imposed by the semantics of the Lua interface - for example, trying to
   -- retrieve a number from a non-numeric value on the stack or using a
   -- reference on a different Lua_State than it was created on.
   Lua_Error : exception;

   -- Special stack positions and the registry
   MaxStack : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_luai_maxstack";
   RegistryIndex : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_registry_index";
   RIDX_MainThread : constant Lua_Integer;
   RIDX_Globals : constant Lua_Integer;
   RIDX_Last : constant Lua_Integer;
   function UpvalueIndex (i : in Integer) return Integer;

   -- Basic state control

   -- Lua_State encapsulates the entire state of a Lua interpreter. Almost every
   -- routine requires a Lua_State to be passed as the first parameter. This
   -- is a Limited_Controlled type internally so it will automatically be
   -- initialised on creation and finalized properly when it goes out of Ada
   -- scope. Every Lua_State contains a main thread of execution, but other
   -- threads may be created with shared global environment, but separate
   -- stacks - see Lua_Thread for details.
   type Lua_State is tagged limited private;

   type Lua_Thread;

   -- This function retrieves the version of the C Lua interpreter that the
   -- program has been compiled against.
   -- Returns a Long_Float value consisting of the Major version * 100 plus the
   -- minor version, so version 5.3 of Lua will return 503.0.
   function Version (L : in Lua_State) return Long_Float;

   -- Status returns the status of a particular interpreter state or thread
   -- using a custom enumeration type.
   function Status (L : in Lua_State) return Thread_Status;

   -- Loads and runs a string containing code. Currently makes a duplicate to
   -- cope with the Ada-C mismatch, so very, very large strings may not work.
   function LoadString (L : in Lua_State;
                        S : in String) return Thread_Status;

   -- Loads and runs a file of a given Name. The Mode parameter allows
   -- specification of whether the file can be in Binary, Text or either format.
   function LoadFile (L : in Lua_State;
                      Name : in String;
                      Mode : in Lua_ChunkMode := Binary_and_Text)
                      return Thread_Status;

   -- Calling, yielding and functions

   -- Calls a function (including an anonymous block of code) on the stack. The
   -- arguments should be pushed onto the stack on top of the function. The
   -- arguments and function will be popped from the stack and replaced with the
   -- results. If nresults is specified using the magic constant
   -- MultRet_Sentinel then a variable number of results can be retrieved. Any
   -- errors will cause the Lua interpreter to panic, which will end the
   -- whole Ada program.
   procedure Call (L : in Lua_State; nargs : in Integer; nresults : in Integer)
     with Inline, Pre => IsFunction(L, -nargs-1);

   -- This procedure looks up the name of a function stored in the global
   -- environment and pushes it onto the stack in the correct place before
   -- invoking Call.
   procedure Call_Function (L : in Lua_State;
                            name : in String;
                            nargs : in Integer;
                            nresults : in Integer);

   -- Calls a function (including an anonymous block of code) on the stack. The
   -- arguments should be pushed onto the stack on top of the function. The
   -- arguments and function will be popped from the stack and replaced with the
   -- results. If nresults is specified using the magic constant
   -- MultRet_Sentinel then a variable number of results can be retrieved. If an
   -- error occurrs then an error message will be pushed to the stack and the
   -- Thread_Status value returned will be an error code. If msgh is non-zero,
   -- it indicates the index of a function on the stack that will be called on
   -- an error which can provide additional debugging infomation.
   function PCall (L : in Lua_State;
                   nargs : in Integer;
                   nresults : in Integer;
                   msgh : in Integer := 0)
                   return Thread_Status
     with Inline, Pre => IsFunction(L, -nargs-1);

   -- This procedure looks up the name of a function stored in the global
   -- environment and pushes it onto the stack in the correct place before
   -- invoking PCall.
   function PCall_Function (L : in Lua_State;
                            name : in String;
                            nargs : in Integer;
                            nresults : in Integer;
                            msgh : in Integer := 0)
                            return Thread_Status;

   -- AdaFunction is used to refer to Ada functions that are suitable for
   -- use by Lua. They are given a Lua_State'Class parameter and other
   -- parameters should be retrieved from the stack. The results should be
   -- pushed onto the stack and the number of results returned.
   -- Note that the function does NOT have to have Convention=>C set.
   type AdaFunction is access function (L : Lua_State'Class) return Natural;

   -- Register an Ada function f under name in the global environment.
   procedure Register(L : in Lua_State; name : in String; f : in AdaFunction);

   -- A magic value that indicates that multiple results may be returned
   MultRet_Sentinel : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_multret";

   -- Pushing values to the stack
   procedure PushAdaClosure (L : in Lua_State; f : in AdaFunction; n : in Natural);
   procedure PushAdaFunction (L : in Lua_State; f : in AdaFunction);
   procedure PushBoolean (L : in  Lua_State; b : in Boolean);
   procedure PushInteger (L : in Lua_State; n : in Lua_Integer);
   procedure PushNil (L : in Lua_State);
   procedure PushNumber (L : in Lua_State; n : in Lua_Number);
   procedure PushString (L : in Lua_State; s : in String);
   function PushThread (L : in Lua_State) return Boolean;
   procedure PushThread (L : in Lua_State);
   procedure SetUserValue (L : in Lua_State; index : in Integer);
   function StringToNumber (L : in Lua_State; s : in String) return Boolean;

   -- Pulling values from the stack
   function ToAdaFunction (L : in Lua_State; index : in Integer) return AdaFunction;
   function ToBoolean (L : in Lua_State; index : in Integer) return Boolean;
   function ToInteger (L : in Lua_State; index : in Integer) return Lua_Integer;
   function ToNumber (L : in Lua_State; index : in Integer) return Lua_Number;
   function ToString (L : in Lua_State; index : in Integer) return String;
   function ToThread (L : in Lua_State; index : in Integer) return Lua_Thread;

   -- Operations on values

   -- Carry out the specified arithmetical or bitwise operation on the top
   -- one or two values on the stack. If the values have special metamethods
   -- defined, they will be used.
   procedure Arith (L : in Lua_State; op : in Arith_Op) with Inline;

   -- Carry out the specified comparison between the two indicated indexes on
   -- the stack. If the values have special metamethods defined, they will be
   -- used. Returns False if either of the indexes is invalid.
   function Compare (L : in Lua_State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean with Inline;

   -- Push the length of the value at the given index onto the stack. Like the
   -- '#' operator, this follows the Lua semantics, so will use the '__len'
   -- metamethod.
   procedure Len (L : in  Lua_State; index : Integer) with Inline;

   -- Compare the two specified indexes without considering the Lua semantics.
   -- Returns False if either of the indexes is invalid.
   function RawEqual (L : in Lua_State; index1, index2 : in Integer)
                      return Boolean with Inline;

   -- Return the length of the value at the given index without considering
   -- the Lua semantics. This does not give a meaningful value for Ada userdata.
   function RawLen (L : in Lua_State; index : Integer)
                    return Integer with Inline;

   -- Garbage Collector control

   -- Instruct the garbage collector to carry out the specified operation.
   procedure GC (L : in Lua_State; what : in GC_Op);

   -- Set a given parameter of the garbage collector and return the previous
   -- value.
   function GC (L : in Lua_State; what : in GC_Param; data : in Integer)
                return Integer;

   -- Return whether the garbage collector is currently running
   -- (i.e. not stopped by a GCSTOP operation)
   function GC_IsRunning (L : in Lua_State) return Boolean;

   -- Stack manipulation and information

   -- Convert an acceptable index into an equivalent absolute index
   function AbsIndex (L : in Lua_State; idx : in Integer) return Integer;

   -- Returns whether the stack can be expanded by at least n extra slots. This
   -- can return False if there is not enough memory or if the maximum stack
   -- size would be exceeded.
   function CheckStack (L : in Lua_State; n : in Integer) return Boolean;

   -- Copy the value at fromidx to toidx, overwriting anything previously at
   -- fromidx.
   procedure Copy (L : in Lua_State; fromidx : in Integer; toidx : in Integer);

   -- Return the index of the top value on the stack. This will also be the
   -- number of items on the stack.
   function GetTop (L : in Lua_State) return Integer;

   -- Move the value at the top of the stack to the (absolute) index value given
   -- moving the values above that point upwards.
   procedure Insert (L : in Lua_State; index : in Integer);

   -- Remove n values from the top of the stack.
   procedure Pop (L : in Lua_State; n : in Integer);

   -- Push a copy of the value at the given index to the top of the stack.
   procedure PushValue (L : in Lua_State; index : in Integer);

   -- Remove the value at the given (absolute) index and move values down to
   -- fill the gap.
   procedure Remove (L : in Lua_State; index : in Integer);

   -- Move the top value on the stack to the index position given, and then
   -- pop the top value.
   procedure Replace (L : in Lua_State; index : in Integer);

   -- Rotate the stack values between idx and the top of the stack by n values
   -- (positive towards the top, negative towards the bottom).
   procedure Rotate (L : in Lua_State; idx : in Integer; n : in Integer);

   -- Set the top of the stack to the given index value. If index is zero, the
   -- result is to empty the stack.
   procedure SetTop (L : in Lua_State; index : in Integer);

   -- Type information
   function IsAdaFunction (L : in Lua_State; index : in Integer) return Boolean;
   function IsBoolean (L : in Lua_State; index : in Integer) return Boolean;
   function IsCFunction (L : in Lua_State; index : in Integer) return Boolean;
   function IsFunction (L : in Lua_State; index : in Integer) return Boolean;
   function IsInteger (L : in Lua_State; index : in Integer) return Boolean;
   function IsLightuserdata (L : in Lua_State; index : in Integer) return Boolean;
   function IsNil (L : in Lua_State; index : in Integer) return Boolean;
   function IsNone (L : in Lua_State; index : in Integer) return Boolean;
   function IsNoneOrNil (L : in Lua_State; index : in Integer) return Boolean;
   function IsNumber (L : in Lua_State; index : in Integer) return Boolean;
   function IsString (L : in Lua_State; index : in Integer) return Boolean;
   function IsTable (L : in Lua_State; index : in Integer) return Boolean;
   function IsThread (L : in Lua_State; index : in Integer) return Boolean;
   function IsUserdata (L : in Lua_State; index : in Integer) return Boolean;
   function TypeInfo (L : in Lua_State; index : in Integer) return Lua_Type;
   function TypeName (L : in Lua_State; tp : in Lua_Type) return String;
   function TypeName (L : in Lua_State; index : in Integer) return String is
     (TypeName(L, TypeInfo(L, index)));
   function Userdata_Name (L : in Lua_State; index : in Integer) return String;

   -- Table manipulation

   -- Create a new empty table and push it onto the stack. 'narr' and 'nrec'
   -- are hints about the number of sequence and non-sequence elements that the
   -- table is expected to have - however they are not limits and can be omitted
   -- if unknown.
   procedure CreateTable (L : in Lua_State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0) with Inline;

   -- Push a new empty table onto the stack.
   procedure NewTable (L : in Lua_State) with Inline;

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined). Returns the type of the pushed value.
   function GetField (L : in Lua_State; index : in Integer; k : in String)
                      return Lua_Type with Inline;

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined). Raises Lua_Error if nothing is found.
   procedure GetField (L : in Lua_State; index : in Integer; k : in String)
     with Inline;

   -- Pushes the value t[i] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined). Returns the type of the pushed value.
   function Geti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
                  return Lua_Type with Inline;

   -- Pushes the value t[i] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined). Raises Lua_Error if nothing is found.
   procedure Geti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
     with Inline;

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined) and k is the value at the top of the stack. Returns
   -- the type of the pushed value.
   function GetTable (L : in Lua_State; index : in Integer) return Lua_Type
     with Inline;

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (or a value of another type with a suitable metamethod for
   -- __index defined) and k is the value at the top of the stack. Raises
   -- Lua_Error if nothing is found.
   procedure GetTable (L : in Lua_State; index : in Integer) with Inline;

   -- Pops a key from the stack and returns the next key-value pair from the
   -- table at the index specified (so stack position -1 will contain the value
   -- and -2 the key). The order in which key-value pairs will be returned is
   -- arbitrary. When using this function to iterate over all the keys in a
   -- table, it is important not to change or mutate the table or unexpected
   -- behaviour can occur. Push a nil value to start the iteration and end the
   -- iteration when Next returns false.
   function Next (L : in Lua_State; index : in Integer)
                  return Boolean with Inline;

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (without using metamethods) and k is the value at the top of
   -- the stack. Returns the type of the pushed value.
   function RawGet (L : in Lua_State; index : in Integer) return Lua_Type
     with Inline, Pre => IsTable(L, index);

   -- Pushes the value t[k] onto the stack, where t is the table at the index
   -- specified (without using metamethods) and k is the value at the top of
   -- the stack. Raises Lua_Error if nothing is found.
   procedure RawGet (L : in Lua_State; index : in Integer)
     with Inline, Pre => IsTable(L, index);

   -- Pushes the value t[i] onto the stack, where t is the table at the index
   -- specified (without using metamethods). Returns the type of the pushed
   -- value.
   function RawGeti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
                     return Lua_Type with Inline, Pre => IsTable(L, index);

   -- Pushes the value t[i] onto the stack, where t is the table at the index
   -- specified (without using metamethods). Raises Lua_Error if nothing is
   -- found.
   procedure RawGeti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
     with Inline, Pre => IsTable(L, index);

   -- Set t[k]=v onto the stack, where t is the table at the index specified
   -- (without using metamethods), v is the value at the top of the stack and
   -- k is the value just below. Both the k and v values are popped from the
   -- stack.
   procedure RawSet (L : in Lua_State; index : in Integer)
     with Inline, Pre => IsTable(L, index);

   -- Set t[i]=v onto the stack, where t is the table at the index specified
   -- (without using metamethods) and v is the value at the top of the stack.
   -- The v value is popped from the stack.
   procedure RawSeti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
     with Inline, Pre => IsTable(L, index);

   -- Set t[k]=v onto the stack, where t is the table at the index specified (or
   -- a value of another type with a suitable metamethod for __newindex defined)
   -- and v is the value at the top of the stack. The v value is popped from the
   -- stack.
   procedure SetField (L : in Lua_State; index : in Integer; k : in String)
     with Inline;

   -- Set t[i]=v onto the stack, where t is the table at the index specified (or
   -- a value of another type with a suitable metamethod for __newindex defined)
   -- and v is the value at the top of the stack. The v value is popped from the
   -- stack.
   procedure Seti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
     with Inline;

   -- Set t[k]=v onto the stack, where t is the table at the index specified
   -- (or a value of another type with a suitable metamethod for __newindex
   -- defined), v is the value at the top of the stack and k is the value
   -- just below. Both the k and v values are popped from the stack.
   procedure SetTable (L : in Lua_State; index : in Integer) with Inline;

   -- Globals and metatables

   -- Push the value of the global with the given name onto the stack and return
   -- its type. A TNIL value is pushed if there is no global with that name.
   function GetGlobal (L : in Lua_State; name : in String) return Lua_Type;

   -- Push the value of the global with the given name onto the stack and return
   -- its type. Raises Lua_Error if there is no global with that name.
   procedure GetGlobal (L : in Lua_State; name : in String);

   -- If the value at the specified index has a metatable, push it to the stack
   -- and return True else return False.
   function GetMetatable (L : in Lua_State; index : in Integer) return Boolean;

   -- If the value at the specified index has a metatable, push it to the stack
   -- and return True else raise Lua_Error.
   procedure GetMetatable (L : in Lua_State; index : in Integer);

   -- Push the global environment table to the stack.
   procedure PushGlobalTable (L : in Lua_State);

   -- Set the global with the given name to the value at the top of the stack.
   procedure SetGlobal (L : in Lua_State; name : in String);

   -- Pop a table from the stack and use it as the metatable for the value at
   -- the given index.
   procedure SetMetatable (L : in Lua_State; index : in Integer);

   -- Threads

   -- A Lua_Thread represents a context in which a coroutine can run. It shares
   -- a global environment with all of the other threads in the Lua_State which
   -- it shares but it has a separate stack. Threads are Lua objects so garbage
   -- collected when not longer referenced by anything in the Lua_State. To
   -- extend the lifetime of threads it may wise to make a reference to them
   -- (see Lua_Reference).
   type Lua_Thread is new Lua_State with private;

   -- This constant is used in calls to Resume to indicate that no thread is
   -- responsible for yielding to this one.
   Null_Thread : constant Lua_Thread;

   -- Returns whether a given Lua_State can yield. The main thread of a
   -- Lua_State cannot yield and neither can threads that have called API
   -- functions. (While the C API gives the possibility of API functions
   -- yielding, this is not supported in the Ada API as the change of
   -- context would not be compatible with the Ada runtime environment).
   function IsYieldable (L : in Lua_State'Class) return Boolean with Inline;

   -- Create and return a new Lua_Thread in a given Lua_State. The new thread
   -- is also pushed onto the stack.
   function NewThread (L : in Lua_State'Class) return Lua_Thread with Inline;

   -- Resume the execution of a thread L with parameters. On the first execution
   -- for a thread the main function is on the stack with its parameters. On
   -- subsequent calls (after the thread has 'yield'ed and the host program has
   -- resumed the thread's execution) the stack should contain the arguments
   -- the thread will see as the result of its 'yield'. The 'from' parameter
   -- contains the thread that yielded to this one, and is not essential.
   function Resume(L : in Lua_State'Class;
                   nargs : in Integer;
                   from : in Lua_State'Class := Null_Thread
                  )
                   return Thread_Status with Inline;

   -- Pop n values from the stack of 'from' and push them to the stack of 'to'.
   procedure XMove (from, to : in Lua_Thread; n : in Integer) with Inline;

   -- Yield a thread and return the specified number of results. When the thread
   -- is 'Resume'd it will continue from the point at which this function was
   -- called. 'nresults' gives the number of results passed to the call of
   -- 'Resume' that resumed the execution of the thread.
   procedure Yield (L : in Lua_State; nresults : Integer) with Inline;

   -- References

   -- Lua_Reference is a reference-counted way of referring to objects inside an
   -- interpreter state.
   type Lua_Reference is tagged private;

   -- Create a reference to the object at the top of the stack. By default the
   -- Lua side of this reference is stored in the Registry but this can be
   -- changed by specifying a reference to a different table through parameter
   -- t.
   function Ref (L : in Lua_State'Class; t : in Integer := RegistryIndex)
                 return Lua_Reference;

   -- Retrieve a Lua object from a reference and return the type of the object.
   function Get (L : in Lua_State; R : Lua_Reference'Class) return Lua_Type;

   -- Retrieve a Lua object from a reference. Raise Lua_Error if the value
   -- cannot be found.
   procedure Get (L : in Lua_State; R : Lua_Reference'Class);

private

   -- Representation clauses

   for Thread_Status use (OK => 0, YIELD => 1, ERRRUN => 2, ERRSYNTAX => 3,
                          ERRMEM => 4, ERRGCMM => 5, ERRERR => 6, ERRFILE => 7);

   for Arith_Op use (OPADD => 0, OPSUB => 1, OPMUL => 2, OPMOD => 3, OPPOW => 4,
                     OPDIV => 5, OPIDIV => 6, OPBAND => 7, OPBOR => 8,
                     OPBXOR => 9, OPSHL => 10, OPSHR => 11, OPUNM => 12,
                     OPBNOT => 13);

   for Comparison_Op use (OPEQ => 0, OPLT => 1, OPLE => 2);

   for Lua_Type use (TNONE => -1, TNIL => 0, TBOOLEAN => 1, TLIGHTUSERDATA => 2,
                     TNUMBER => 3, TSTRING => 4, TTABLE => 5, TFUNCTION => 6,
                     TUSERDATA => 7, TTHREAD => 8, TNUMTAGS => 9);

   -- Deferred constants
   RIDX_MainThread : constant Lua_Integer := 1;
   RIDX_Globals : constant Lua_Integer := 2;
   RIDX_Last : constant Lua_Integer := RIDX_Globals;

   -- Main Lua_State type and derivatives
   subtype void_ptr is System.Address;

   type Lua_State is new Ada.Finalization.Limited_Controlled with
      record
         L : void_ptr;
      end record;

   overriding procedure Initialize (Object : in out Lua_State) with inline;
   overriding procedure Finalize   (Object : in out Lua_State) with inline;

   -- Existing_State is a clone of State but without automatic initialization
   -- It is used internally when lua_State* are returned from the Lua library
   -- and we don't want to re-initialize them when turning them into the
   -- State record type.
   type Existing_State is new Lua_State with null record;
   overriding procedure Initialize (Object : in out Existing_State) is null;
   overriding procedure Finalize   (Object : in out Existing_State) is null;

   type Lua_Thread is new Existing_State with null record;

   Null_Thread : constant Lua_Thread
     := (Ada.Finalization.Limited_Controlled with L => System.Null_Address);

   -- Trampolines

   function CFunction_Trampoline (L : System.Address) return Interfaces.C.int
     with Convention => C;

   -- References

   type Lua_Reference_Value is
      record
         State : void_ptr;
         Table : Interfaces.C.int;
         Ref : Interfaces.C.int;
         Count : Natural := 0;
      end record;

   type Lua_Reference_Value_Access is access Lua_Reference_Value;

   type Lua_Reference is
     new Ada.Finalization.Controlled with
      record
         E : Lua_Reference_Value_Access := null;
      end record;

   overriding procedure Initialize (Object : in out Lua_Reference) is null;
   overriding procedure Adjust (Object : in out Lua_Reference);
   overriding procedure Finalize (Object : in out Lua_Reference);

end Lua;
