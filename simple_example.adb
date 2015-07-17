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

with Lua; use Lua;

with Simple_Example_Foobar;

procedure Simple_Example is
   L : State;
   Success : Thread_Status;

   procedure Print_Stack(L : State) is
   begin
      Put_Line("Relative index : Absolute index : Type : Contents");
      for I in reverse 1..L.GetTop loop
         Put(I - L.GetTop - 1); Put(" : ");
         Put(L.AbsIndex(I)); Put(" : ");
         Put(L.TypeName(L.TypeInfo(I))); Put(" : ");
         case L.TypeInfo(I) is
            when TBOOLEAN =>
               Put((if L.ToBoolean(I) then "true" else "false"));
            when TNUMBER =>
               Put(L.ToNumber(I), Aft => 0, Exp => 0);
            when TSTRING =>
               Put("'" & L.ToString(I) & "'");
            when others =>
               Put("-");
         end case;
         New_Line;
      end loop;
   end Print_Stack;

begin
   Put_Line("A simple example of using Lua from within Ada");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line;

   Put_Line("Lua state status: " & Thread_Status'Image(L.Status));
   New_Line;

   Put_Line("Basic stack manipulation.");
   Put("Initial stack size: "); Put(L.GetTop); New_Line;
   Put_Line("Pushing 3.0, 7.5, 2.3, 'Hello, World!', True, 5");
   L.PushNumber(3.0);
   L.PushNumber(7.5);
   L.PushNumber(2.3);
   L.PushString("Hello, World!");
   L.PushBoolean(True);
   L.PushInteger(5);
   Put("Stack size now: "); Put(L.GetTop); New_Line;
   Put_Line("Stack now contains:");
   Print_Stack(L);
   Put("Get top element as string: "); Put(L.ToString(-1)); New_Line;
   Put_Line("Stack now contains:");
   Print_Stack(L);
   Put_Line("Pop top three elements");
   L.Pop(3);
   Put_Line("Stack now contains:");
   Print_Stack(L);
   Put_Line("Is Stack(-2) <= Stack(-1)? " &
            (if L.Compare(-2, -1, OPLE) then "Yes" else "No"));
   Put_Line("Duplicating element at index 2. Stack now contains:");
   L.PushValue(2);
   Print_Stack(L);
   Put_Line("Adding top two elements. Stack now contains:");
   L.Arith(OPADD);
   Print_Stack(L);
   Put_Line("Is Stack(-2) <= Stack(-1)? " &
            (if L.Compare(-2, -1, OPLE) then "Yes" else "No"));
   New_Line;

   Put_Line("Setting global foobar to 5");
   L.PushNumber(5.0);
   L.Setglobal("foobar");
   L.GetGlobal("foobar");
   Put("Global foobar = "); Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   Put_Line("Loading chunk: function f (x) return 2*x end");
   Success := L.LoadString("function f (x) return 2*x end");
   Put_Line("Load" & (if Success /= OK then " not" else "") & " successful.");
   L.Call(0, 0);
   Put_Line("Compiled chunk.");
   Put("Result of calling f (3): ");
   L.GetGlobal("f"); L.PushNumber(3.0); L.Call(1, 1);
   Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   L.Pop(1);
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction foobar in Lua");
   L.Register("foobar", AdaFunction'(Simple_Example_Foobar'Access));
   Success := L.LoadString("baz = foobar(5.0)");
   Put_Line("Loading code snippet 'baz = foobar(5.0)'" &
            (if Success /= OK then " not" else "") & " successful.");
   Put_Line("Calling 'baz = foobar(5.0)' from Lua");
   L.Call(0, 0);
   Put("baz = ");
   L.GetGlobal("baz");
   Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   Put_Line("Manually triggering garbage collection...");
   L.GC(GCCOLLECT);
   New_Line;

   Put("Checking type of main thread: ");
   L.geti(RegistryIndex, RIDX_MainThread);
   Put(L.TypeName(L.TypeInfo(-1)));
   New_Line;

end Simple_Example;
