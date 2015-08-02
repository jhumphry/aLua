-- Functions_Example
-- A example of using the Ada 2012 interface to Lua for functions / closures etc

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Characters.Latin_1;

with Lua; use Lua;
with Lua.Libs;
with Lua.Util; use Lua.Util;

with Example_AdaFunctions;

procedure Functions_Example is
   L : Lua_State;
   Success : Thread_Status;
   R : Lua_Reference;

   LF : Character renames Ada.Characters.Latin_1.LF;
   Coroutine_Source : constant String := "" &
     "function co (x) " & LF &
     " for i = 1, x do " & LF &
     "  yield(i) " & LF &
     " end " & LF &
     " return -1 " & LF &
     " end " & LF &
     "";

begin

   Put_Line("A simple example of using Lua and Ada functions together");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line; New_Line;

   Put_Line("Loading chunk: function f (x) return 2*x end");
   Success := L.LoadString("function f (x) return 2*x end");
   Put_Line("Load" & (if Success /= OK then " not" else "") & " successful.");
   L.Call(nargs => 0, nresults =>0);
   Put_Line("Compiled chunk.");
   Put("Result of calling f (3): ");
   L.PushInteger(3);
   L.Call_Function(name => "f", nargs => 1, nresults => 1);
   Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   Put_Line("Saving a reference to the top of the stack and clearing it.");
   R := Ref(L);
   L.Pop(L.GetTop);
   Print_Stack(L);
   Put_Line("Retrieving reference...");
   L.Get(R);
   Print_Stack(L);
   New_Line;

   Put_Line("Loading IO library and running example Lua file");
   Libs.Require_Standard_Library(L, LIbs.IO_Lib);
   Success := L.LoadFile("examples/example_lua.lua");
   Put_Line("Load" & (if Success /= OK then " not" else "") & " successful.");
   if Success = OK then
      L.Call(nargs => 0, nresults =>0);
      Put_Line("Compiled chunk. Result of calling triangle (5):");
      L.PushNumber(5.0);
      L.Call_Function(name => "triangle", nargs => 1, nresults => 0);
   end if;
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction foobar in Lua");
   L.Register("foobar", AdaFunction'(Example_AdaFunctions.Foobar'Access));
   Success := L.LoadString("baz = foobar(5.0)");
   Put_Line("Loading code snippet 'baz = foobar(5.0)'" &
            (if Success /= OK then " not" else "") & " successful.");
   Put_Line("Calling 'baz = foobar(5.0)' from Lua");
   L.Call(nargs => 0, nresults =>0);
   Put("baz = ");
   L.GetGlobal("baz");
   Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Checking foobar can be retrieved");
   L.GetGlobal("foobar");
   if not L.IsAdaFunction(-1) then
      Put_Line("Error - foobar does not contain an AdaFunction?");
   end if;
   if L.ToAdaFunction(-1) = AdaFunction'(Example_AdaFunctions.Foobar'Access) then
      Put_Line("AdaFunction foobar retrieved successfully from Lua");
   else
      Put_Line("AdaFunction foobar was NOT retrieved from Lua");
   end if;
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction multret in Lua");
   L.Register("multret", AdaFunction'(Example_AdaFunctions.Multret'Access));
   Put_Line("Calling 'multret(5)' from Lua");
   L.PushInteger(5);
   L.Call_Function(name => "multret", nargs => 1, nresults => MultRet_Sentinel);
   Print_Stack(L);
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Registering an AdaFunction closure (with upvalue 2.0) in Lua");
   L.PushNumber(2.0);
   L.PushAdaClosure(AdaFunction'(Example_AdaFunctions.Closure'Access), 1);
   L.SetGlobal("closure");
   Put_Line("Calling 'closure(3.5)' from Lua");
   L.PushNumber(3.5);
   L.Call_Function(name => "closure", nargs => 1, nresults => MultRet_Sentinel);
   Print_Stack(L);
   New_Line;

   L.Pop(L.GetTop);
   Put_Line("Now to look at using coroutines");
   Libs.Add_Yield_Function(L);
   Put_Line("Loading coroutine source: ");
   Put_Line(Coroutine_Source);
   Success := L.LoadString(Coroutine_Source);
   Put_Line("Load" & (if Success /= OK then " not" else "") & " successful.");
   L.Call(nargs => 0, nresults =>0);
   Put_Line("Compiled coroutine code.");
   declare
      Coroutine : constant Lua_Thread := L.NewThread;
      Coroutine_Status : Thread_Status;
   begin
      Put_Line("New thread created");
      Put_Line("Resuming coroutine with parameter 3 in this thread:");
      Coroutine.GetGlobal("co");
      Coroutine.PushInteger(3);
      Coroutine_Status := Coroutine.resume(from => L, nargs => 1);
      Put("Coroutine status : " & Thread_Status'Image(Coroutine_Status));
      Put(" Result: "); Put(Coroutine.ToNumber(-1)); New_Line;
      L.Pop(1); -- argument no longer needed in this example
      while Coroutine_Status = YIELD loop
         Coroutine_Status := Coroutine.resume(from => L, nargs => 0);
         Put("Coroutine status : " & Thread_Status'Image(Coroutine_Status));
         Put(" Result: "); Put(Coroutine.ToNumber(-1)); New_Line;
      end loop;
   end;
   New_Line;

   L.Pop(L.GetTop);
   declare
      S : constant Lua_Reference := R;
   begin
      Put_Line("Retrieving a copy of reference saved earlier...");
      L.Get(S);
      Print_Stack(L);
   end;
   Put_Line("Retrieving reference saved earlier...");
   L.Get(R);
   Print_Stack(L);
   New_Line;

end Functions_Example;
