-- Lua

-- An Ada 2012 interface to the Lua language

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

package Lua is

   subtype Number is Long_Float;

   type Thread_Status is (OK, YIELD, ERRRUN, ERRSYNTAX, ERRMEM, ERRGCMM, ERRERR);

   type Arith_Op is (OPADD, OPSUB, OPMUL, OPMOD, OPPOW, OPDIV, OPIDIV, OPBAND,
                     OPBOR, OPBXOR, OPSHL, OPSHR, OPUNM, OPBNOT);

   type Comparison_Op is (OPEQ, OPLT, OPLE);

   type GC_Op is (GCSTOP, GCRESTART, GCCOLLECT, GCCOUNT, GCCOUNTB, GCSTEP);

   type GC_Param is (GCSETPAUSE, GCSETSTEPMUL);

   type Lua_Type is (TNONE, TNIL, TBOOLEAN, TLIGHTUSERDATA, TNUMBER, TSTRING,
                     TTABLE, TFUNCTION, TUSERDATA, TTHREAD, TNUMTAGS);

   -- Imports to do with special stack positions and the registry
   MaxStack : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_luai_maxstack";
   RegistryIndex : constant Integer
     with Import, Convention => C, Link_Name => "lua_conf_registry_index";
   RIDX_MainThread : constant Integer;
   RIDX_Globals : constant Integer;
   RIDX_Last : constant Integer;
   function UpvalueIndex (i : in Integer) return Integer;

   type State is tagged limited private;
   function Version (L : in State) return Long_Float;
   function Status (L : in State) return Thread_Status;
   procedure Push (L : in out State; n : in Number);
   function ToNumber (L : in State; index : in Integer) return Number;
   procedure Arith (L : in out State; op : in Arith_Op);
   function Compare (L : in State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean;
   procedure GC (L : in State; what : in GC_Op);
   function GC (L : in State; what : in GC_Param; data : in Integer)
                return Integer;
   function GC (L : in State) return Boolean;

   -- Stack manipulation and information
   function AbsIndex (L : in State; idx : in Integer) return Integer;
   function CheckStack (L : in State; n : in Integer) return Boolean;
   procedure Copy (L : in out State; fromidx : in Integer; toidx : in Integer);
   function GetTop (L : in State) return Integer;
   procedure Insert (L : in out State; index : in Integer);
   procedure Pop (L : in out State; n : in Integer);
   procedure PushValue (L : in out State; index : in Integer);
   procedure Remove (L : in out State; index : in Integer);
   procedure Replace (L : in out State; index : in Integer);
   procedure Rotate (L : in out State; idx : in Integer; n : in Integer);
   procedure SetTop (L : in out State; index : in Integer);

   -- Type information
   function TypeInfo (L : in State; index : in Integer) return Lua_Type;
   function TypeName (L : in State; tp : in Lua_Type) return String;

   -- Table manipulation
   procedure createtable (L : in out State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0);
   procedure newtable (L : in out State);
   function getfield (L : in out State; index : in Integer; k : in String)
                  return Lua_Type;
   procedure getfield (L : in out State; index : in Integer; k : in String);
   function geti (L : in out State; index : in Integer; i : in Integer)
                  return Lua_Type;
   procedure geti (L : in out State; index : in Integer; i : in Integer);
   function gettable (L : in out State; index : in Integer) return Lua_Type;
   procedure gettable (L : in out State; index : in Integer);
   function next (L : in out State; index : in Integer) return Boolean;
   procedure setfield (L : in out State; index : in Integer; k : in String);
   procedure seti (L : in out State; index : in Integer; i : in Integer);
   procedure settable (L : in State; index : in Integer);

private

   subtype void_ptr is System.Address;

   for Thread_Status use (OK => 0, YIELD => 1, ERRRUN => 2, ERRSYNTAX => 3,
                          ERRMEM => 4, ERRGCMM => 5, ERRERR => 6);

   for Arith_Op use (OPADD => 0, OPSUB => 1, OPMUL => 2, OPMOD => 3, OPPOW => 4,
                     OPDIV => 5, OPIDIV => 6, OPBAND => 7, OPBOR => 8,
                     OPBXOR => 9, OPSHL => 10, OPSHR => 11, OPUNM => 12,
                     OPBNOT => 13);

   for Comparison_Op use (OPEQ => 0, OPLT => 1, OPLE => 2);

   for GC_Op use (GCSTOP => 0, GCRESTART => 1, GCCOLLECT => 2, GCCOUNT => 3,
                  GCCOUNTB => 4, GCSTEP => 5);

   for GC_Param use (GCSETPAUSE => 6, GCSETSTEPMUL => 7);

   GCISRUNNING : constant := 9;

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

end Lua;
