-- Simple_Example
-- A simple example of using the Ada 2012 interface to Lua

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Lua; use Lua;
with Lua.Util; use Lua.Util;

procedure Simple_Example is
   L : Lua_State;

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
   Print_Stack(L);
   Put("Get top element as string: "); Put(L.ToString(-1)); New_Line;
   Print_Stack(L);
   Put_Line("Pop top three elements");
   L.Pop(3);
   Print_Stack(L);
   Put_Line("Is Stack(-2) <= Stack(-1)? " &
            (if L.Compare(index1 => -2, index2 => -1, op => OPLE)
               then "Yes"
               else "No"));
   Put_Line("Duplicating element at index 2. ");
   L.PushValue(2);
   Print_Stack(L);
   Put_Line("Adding top two elements.");
   L.Arith(OPADD);
   Print_Stack(L);
   Put_Line("Is Stack(-2) <= Stack(-1)? " &
            (if L.Compare(index1 => -2, index2 => -1, op => OPLE)
               then "Yes"
               else "No"));
   New_Line;

   Put_Line("Setting global foobar to 5");
   L.PushNumber(5.0);
   L.SetGlobal("foobar");
   L.GetGlobal("foobar");
   Put("Global foobar = "); Put(L.ToNumber(-1), Aft => 0, Exp => 0); New_Line;
   New_Line;

   Put_Line("Manually triggering garbage collection...");
   L.GC(what => GCCOLLECT);
   New_Line;

   Put("Checking type of main thread: ");
   L.Geti(index => RegistryIndex, i => RIDX_MainThread);
   Put(L.TypeName(-1));
   New_Line;

end Simple_Example;
