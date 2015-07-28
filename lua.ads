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

   subtype Lua_Number is Long_Float;
   subtype Lua_Integer is Long_Long_Integer;

   type Thread_Status is (OK, YIELD, ERRRUN, ERRSYNTAX, ERRMEM, ERRGCMM, ERRERR);

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

   Lua_Error : exception;

   -- Special stack positions and the registry
   MaxStack : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_luai_maxstack";
   RegistryIndex : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_registry_index";
   RIDX_MainThread : constant Integer;
   RIDX_Globals : constant Integer;
   RIDX_Last : constant Integer;
   function UpvalueIndex (i : in Integer) return Integer;

   -- Basic state control
   type State is tagged limited private;
   type Thread;
   function Version (L : in State) return Long_Float;
   function Status (L : in State) return Thread_Status;
   function LoadString (L : in State;
                        S : in String) return Thread_Status;
   function LoadFile (L : in State;
                      Name : in String;
                      Mode : in Lua_ChunkMode := Binary_and_Text)
                      return Thread_Status;

   -- Calling, yielding and functions
   procedure Call (L : in State; nargs : in Integer; nresults : in Integer);
   function PCall (L : in State;
                   nargs : in Integer;
                   nresults : in Integer;
                   msgh : in Integer := 0)
                   return Thread_Status;
   type AdaFunction is access function (L : State'Class) return Natural;
   procedure Register(L : in State; name : in String; f : in AdaFunction);
   MultRet_Sentinel : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_multret";

   -- Pushing values to the stack
   procedure PushAdaClosure (L : in State; f : in AdaFunction; n : in Natural);
   procedure PushAdaFunction (L : in State; f : in AdaFunction);
   procedure PushBoolean (L : in  State; b : in Boolean);
   procedure PushInteger (L : in State; n : in Lua_Integer);
   procedure PushNil (L : in State);
   procedure PushNumber (L : in State; n : in Lua_Number);
   procedure PushString (L : in State; s : in String);
   function PushThread (L : in State) return Boolean;
   procedure PushThread (L : in State);
   function StringToNumber (L : in State; s : in String) return Boolean;

   -- Pulling values from the stack
   function ToAdaFunction (L : in State; index : in Integer) return AdaFunction;
   function ToBoolean (L : in State; index : in Integer) return Boolean;
   function ToInteger (L : in State; index : in Integer) return Lua_Integer;
   function ToNumber (L : in State; index : in Integer) return Lua_Number;
   function ToString (L : in State; index : in Integer) return String;
   function ToThread (L : in State; index : in Integer) return Thread;

   -- Operations on values
   procedure Arith (L : in State; op : in Arith_Op);
   function Compare (L : in State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean;
   procedure Len (L : in  State; index : Integer);
   function RawEqual (L : in State; index1, index2 : in Integer) return Boolean;
   function RawLen (L : in State; index : Integer) return Integer;

   -- Garbage Collector control
   procedure GC (L : in State; what : in GC_Op);
   function GC (L : in State; what : in GC_Param; data : in Integer)
                return Integer;
   function GC (L : in State) return Boolean;

   -- Stack manipulation and information
   function AbsIndex (L : in State; idx : in Integer) return Integer;
   function CheckStack (L : in State; n : in Integer) return Boolean;
   procedure Copy (L : in State; fromidx : in Integer; toidx : in Integer);
   function GetTop (L : in State) return Integer;
   procedure Insert (L : in State; index : in Integer);
   procedure Pop (L : in State; n : in Integer);
   procedure PushValue (L : in State; index : in Integer);
   procedure Remove (L : in State; index : in Integer);
   procedure Replace (L : in State; index : in Integer);
   procedure Rotate (L : in State; idx : in Integer; n : in Integer);
   procedure SetTop (L : in State; index : in Integer);

   -- Type information
   function IsAdaFunction (L : in State; index : in Integer) return Boolean;
   function IsBoolean (L : in State; index : in Integer) return Boolean;
   function IsCFunction (L : in State; index : in Integer) return Boolean;
   function IsFunction (L : in State; index : in Integer) return Boolean;
   function IsInteger (L : in State; index : in Integer) return Boolean;
   function IsLightuserdata (L : in State; index : in Integer) return Boolean;
   function IsNil (L : in State; index : in Integer) return Boolean;
   function IsNone (L : in State; index : in Integer) return Boolean;
   function IsNoneOrNil (L : in State; index : in Integer) return Boolean;
   function IsNumber (L : in State; index : in Integer) return Boolean;
   function IsString (L : in State; index : in Integer) return Boolean;
   function IsTable (L : in State; index : in Integer) return Boolean;
   function IsThread (L : in State; index : in Integer) return Boolean;
   function IsUserdata (L : in State; index : in Integer) return Boolean;
   function TypeInfo (L : in State; index : in Integer) return Lua_Type;
   function TypeName (L : in State; tp : in Lua_Type) return String;
   function TypeName (L : in State; index : in Integer) return String is
     (TypeName(L, Typeinfo(L, index)));


   -- Table manipulation
   procedure createtable (L : in State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0);
   procedure newtable (L : in State);
   function getfield (L : in State; index : in Integer; k : in String)
                  return Lua_Type;
   procedure getfield (L : in State; index : in Integer; k : in String);
   function geti (L : in State; index : in Integer; i : in Integer)
                  return Lua_Type;
   procedure geti (L : in State; index : in Integer; i : in Integer);
   function gettable (L : in State; index : in Integer) return Lua_Type;
   procedure gettable (L : in State; index : in Integer);
   function next (L : in State; index : in Integer) return Boolean;
   function rawget (L : in State; index : in Integer) return Lua_Type;
   procedure rawget (L : in State; index : in Integer);
   function rawgeti (L : in State; index : in Integer; i : in Integer)
                  return Lua_Type;
   procedure rawgeti (L : in State; index : in Integer; i : in Integer);
   procedure rawset (L : in State; index : in Integer);
   procedure rawseti (L : in State; index : in Integer; i : in Integer);
   procedure setfield (L : in State; index : in Integer; k : in String);
   procedure seti (L : in State; index : in Integer; i : in Integer);
   procedure settable (L : in State; index : in Integer);

   -- Globals and metatables
   function getglobal (L : in State; name : in String) return Lua_Type;
   procedure getglobal (L : in State; name : in String);
   function getmetatable (L : in State; index : in Integer) return Boolean;
   procedure getmetatable (L : in State; index : in Integer);
   procedure pushglobaltable (L : in State);
   procedure setglobal (L : in State; name : in String);
   procedure setmetatable (L : in State; index : in Integer);

   -- Threads
   type Thread is new State with private;
   function isyieldable (L : in State'Class) return Boolean;
   function newthread (L : in State'Class) return Thread;
   function resume(L : in State'Class; from : in State'Class; nargs : Integer)
     return Thread_Status;
   procedure xmove (from, to : in Thread; n : in Integer);
   procedure yield (L : in State; nresults : Integer);

   -- References
   type Lua_Reference is tagged private;
   function Ref (L : in State'Class; t : in Integer := RegistryIndex)
                 return Lua_Reference;
   function Get (L : in State; R : Lua_Reference'Class) return Lua_Type;
   procedure Get (L : in State; R : Lua_Reference'Class);

private

   subtype void_ptr is System.Address;

   for Thread_Status use (OK => 0, YIELD => 1, ERRRUN => 2, ERRSYNTAX => 3,
                          ERRMEM => 4, ERRGCMM => 5, ERRERR => 6);

   for Arith_Op use (OPADD => 0, OPSUB => 1, OPMUL => 2, OPMOD => 3, OPPOW => 4,
                     OPDIV => 5, OPIDIV => 6, OPBAND => 7, OPBOR => 8,
                     OPBXOR => 9, OPSHL => 10, OPSHR => 11, OPUNM => 12,
                     OPBNOT => 13);

   for Comparison_Op use (OPEQ => 0, OPLT => 1, OPLE => 2);

   for Lua_Type use (TNONE => -1, TNIL => 0, TBOOLEAN => 1, TLIGHTUSERDATA => 2,
                     TNUMBER => 3, TSTRING => 4, TTABLE => 5, TFUNCTION => 6,
                     TUSERDATA => 7, TTHREAD => 8, TNUMTAGS => 9);

   RIDX_MainThread : constant Integer := 1;
   RIDX_Globals : constant Integer := 2;
   RIDX_Last : constant Integer := RIDX_Globals;

   type State is new Ada.Finalization.Limited_Controlled with
      record
         L : void_Ptr;
      end record;

   overriding procedure Initialize (Object : in out State);
   overriding procedure Finalize   (Object : in out State);

   -- Existing_State is a clone of State but without automatic initialization
   -- It is used internally when lua_State* are returned from the Lua library
   -- and we don't want to re-initialize them when turning them into the
   -- State record type.
   type Existing_State is new State with null record;
   overriding procedure Initialize (Object : in out Existing_State) is null;
   overriding procedure Finalize   (Object : in out Existing_State) is null;

   type Thread is new Existing_State with null record;

   -- Trampolines

   function CFunction_Trampoline (L : System.Address) return Interfaces.C.int
     with Convention => C;

   -- References

   type Lua_Reference_Value is
      record
         State : void_Ptr;
         Table : Interfaces.C.Int;
         Ref : Interfaces.C.Int;
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
