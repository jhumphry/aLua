-- Simple_Example

-- A simple example of using the Ada 2012 interface to Lua

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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Lua;

procedure Simple_Example is
   L : Lua.State;

   procedure Print_Stack(L : Lua.State) is
   begin
      Put_Line("Relative index : Absolute index : Contents");
      for I in 1..L.GetTop loop
         Put(-I); Put(" : "); Put(L.AbsIndex(-I)); Put(" : ");
         Put(L.ToNumber(-I), Aft => 0, Exp => 0); New_Line;
      end loop;
   end Print_Stack;

begin
   Put_Line("A simple example of using Lua from within Ada");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line;

   Put_Line("Lua state status: " & Lua.Thread_Status'Image(L.Status));
   New_Line;

   Put_Line("Basic stack manipulation.");
   Put("Initial stack size: "); Put(L.GetTop); New_Line;
   Put_Line("Pushing 3.0, 7.5, 2.3");
   L.Push(3.0); L.Push(7.5); L.Push(2.3);
   Put("Stack size now: "); Put(L.GetTop); New_Line;
   Put_Line("Stack now contains:");
   Print_Stack(L);
   Put_Line("Duplicating element at index 2");
   L.PushValue(2);
   Put_Line("Stack now contains:");
   Print_Stack(L);

end Simple_Example;
