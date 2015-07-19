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

with Lua; use Lua;
with Lua.Util; use Lua.Util;

with Func_Foobar;
with Func_Multret;
with Func_Closure;

procedure Functions_Example is
   L : State;
   Success : Thread_Status;

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
   L.Register("foobar", AdaFunction'(Func_Foobar'Access));
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
   L.Register("multret", AdaFunction'(Func_Multret'Access));
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
   L.PushAdaClosure(AdaFunction'(Func_Closure'Access), 1);
   L.SetGlobal("closure");
   Put_Line("Calling 'closure(3.5)' from Lua");
   L.GetGlobal("closure");
   L.PushNumber(3.5);
   L.Call(1, MultRet_Sentinel);
   Put_Line("Stack now contains:");
   Print_Stack(L);
   New_Line;

end Functions_Example;
