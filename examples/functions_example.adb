-- Functions_Example

-- A example of using the Ada 2012 interface to Lua for functions / closures etc

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
with Ada.Characters.Latin_1;

with Lua; use Lua;
with Lua.Util; use Lua.Util;

with Example_AdaFunctions;

procedure Functions_Example is
   L : State;
   Success : Thread_Status;

   LF : Character renames Ada.Characters.Latin_1.LF;
   Coroutine_Source : String := "" &
     "co = coroutine.create( function  (x) " & LF &
     " for i = 1, x do " & LF &
     "  coroutine.yield(i) " & LF &
     " end " & LF &
     " return -1 " & LF &
     " end " & LF &
     ")";

begin

   Put_Line("A simple example of using Lua and Ada functions together");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line; New_Line;

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
   L.Register("foobar", AdaFunction'(Example_AdaFunctions.Foobar'Access));
   Success := L.LoadString("baz = foobar(5.0)");
   Put_Line("Loading code snippet 'baz = foobar(5.0)'" &
            (if Success /= OK then " not" else "") & " successful.");
   Put_Line("Calling 'baz = foobar(5.0)' from Lua");
   L.Call(0, 0);
   Put("baz = ");
   L.GetGlobal("baz");
   Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction multret in Lua");
   L.Register("multret", AdaFunction'(Example_AdaFunctions.Multret'Access));
   Put_Line("Calling 'multret(5)' from Lua");
   L.GetGlobal("multret");
   L.PushInteger(5);
   L.Call(1, MultRet_Sentinel);
   Put_Line("Stack now contains:");
   Print_Stack(L);
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction closure (with upvalue 2.0) in Lua");
   L.PushNumber(2.0);
   L.PushAdaClosure(AdaFunction'(Example_AdaFunctions.Closure'Access), 1);
   L.SetGlobal("closure");
   Put_Line("Calling 'closure(3.5)' from Lua");
   L.GetGlobal("closure");
   L.PushNumber(3.5);
   L.Call(1, MultRet_Sentinel);
   Put_Line("Stack now contains:");
   Print_Stack(L);
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Adding standard libraries...");
   L.OpenLibs;
   Put_Line("Loading coroutine: " & Coroutine_Source); New_Line;
   Success := L.LoadString(Coroutine_Source);
   Put_Line("Load" & (if Success /= OK then " not" else "") & " successful.");
   L.Call(0, 0);
   L.GetGlobal("co");
   pragma Assert (L.TypeInfo(-1) = TTHREAD);
   Put_Line("Compiled chunk. Resuming coroutine with parameter 3");
   declare
      Coroutine : Thread := L.ToThread(-1);
      Coroutine_Status : Thread_Status;
   begin
      Coroutine.PushInteger(3);
      Coroutine_Status := Coroutine.resume(L, 1);
      Put_Line("Coroutine status : " & Thread_Status'Image(Coroutine_Status));
      Print_Stack(Coroutine);
      while Coroutine_Status = YIELD loop
         Coroutine_Status := Coroutine.resume(L, 1);
         Put_Line("Coroutine status : " & Thread_Status'Image(Coroutine_Status));
         Print_Stack(Coroutine);
      end loop;
   end;
   New_Line;

end Functions_Example;
