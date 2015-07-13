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

   type Lua_Type is (TNONE, TNIL, TBOOLEAN, TLIGHTUSERDATA, TNUMBER, TSTRING,
                     TTABLE, TFUNCTION, TUSERDATA, TTHREAD, TNUMTAGS);

   type State is tagged limited private;
   function Version (L : in State) return Long_Float;
   function Status (L : in State) return Thread_Status;
   procedure Push (L : in out State; n : in Number);
   function ToNumber (L : in State; index : in Integer) return Number;
   procedure Arith (L : in out State; op : in Arith_Op);

   -- Stack manipulation and information
   function AbsIndex (L : in State; idx : in Integer) return Integer;
   function CheckStack (L : in State; n : in Integer) return Boolean;
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

private

   subtype void_ptr is System.Address;

   for Thread_Status use (OK => 0, YIELD => 1, ERRRUN => 2, ERRSYNTAX => 3,
                          ERRMEM => 4, ERRGCMM => 5, ERRERR => 6);

   for Arith_Op use (OPADD => 0, OPSUB => 1, OPMUL => 2, OPMOD => 3, OPPOW => 4,
                     OPDIV => 5, OPIDIV => 6, OPBAND => 7, OPBOR => 8,
                     OPBXOR => 9, OPSHL => 10, OPSHR => 11, OPUNM => 12,
                     OPBNOT => 13);

   for Lua_Type use (TNONE => -1, TNIL => 0, TBOOLEAN => 1, TLIGHTUSERDATA => 2,
                     TNUMBER => 3, TSTRING => 4, TTABLE => 5, TFUNCTION => 6,
                     TUSERDATA => 7, TTHREAD => 8, TNUMTAGS => 9);

   type State is new Ada.Finalization.Limited_Controlled with
      record
         L : void_Ptr;
      end record;

   overriding procedure Initialize (Object : in out State);
   overriding procedure Finalize   (Object : in out State);

end Lua;
