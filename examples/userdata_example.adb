-- Userdata_Example
-- A example of using the Ada values in Lua via UserData values

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;

with Lua; use Lua;
with Lua.Libs;
with Lua.Util; use Lua.Util;

with Example_Userdata;
use Example_Userdata;

procedure Userdata_Example is
   L : Lua_State;
   Result : Boolean;
   Success :Thread_Status;

   Example_Value : aliased Example := (Number => 5, Flag => True);

begin

   Put_Line("Using Ada values as userdata in Lua");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line; New_Line;

   Put_Line("Creating a metatable and adding a 'toggle' command.");
   Register_Operations(L);
   New_Line;

   Put_Line("Pushing an 'Example' value (Number => 5, Flag => True) to the stack.");
   Userdata_Package.Push(L, Example_Value'Unchecked_Access);
   Print_Stack(L);
   Put_Line("Retrieving the value of 'Flag' from the value at the top of the stack.");
   Result := Userdata_Package.ToUserdata(L, -1).flag;
   Put_Line((if Result then "Flag is true as expected"
            else "Error: flag is not true?"));
   Put_Line("Saving userdata in global 'foo'");
   L.SetGlobal("foo");
   New_Line;

   Put_Line("Calling 'foo:toggle()' in Lua");
   Success := L.LoadString("foo:toggle()");
   L.Call(0, 0);
   New_Line;

   Put_Line("Retrieving the value of 'Flag' from the global foo.");
   L.GetGlobal("foo");
   Result := Userdata_Package.ToUserdata(L, -1).flag;
   Put_Line((if Result then "Flag is now true"
            else "Flag is now false"));
   New_Line;

end Userdata_Example;
