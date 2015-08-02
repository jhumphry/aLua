-- Userdata_Example
-- A example of using the Ada values in Lua via UserData values

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;

with Lua; use Lua;
with Lua.Util; use Lua.Util;

with Example_Userdata;
use Example_Userdata;

procedure Userdata_Example is
   L : Lua_State;
   Result : Boolean;
   Counter : Integer;
   Discard :Thread_Status;

   Example_Object : aliased Example_Parent := (Counter => 5, Flag => True);
   Example_Child_Object : aliased Example_Child := (Counter => 3, Flag => False);

begin

   Put_Line("Using Ada values as userdata in Lua");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line; New_Line;

   Put_Line("Creating a metatable and adding a 'toggle' command.");
   Register_Operations(L);
   New_Line;

   Put_Line("Pushing an 'Example_Parent' value (Counter => 5, Flag => True) to the stack.");
   Userdata_Package.Push(L, Example_Object'Unchecked_Access);
   Put_Line("Saving a copy of the userdata in global 'foo'");
   L.PushValue(-1);
   L.SetGlobal("foo");
   Print_Stack(L);
   Put_Line("Retrieving the value of 'Flag' from the value at the top of the stack.");
   Result := Userdata_Package.ToUserdata(L, -1).Flag;
   Put_Line((if Result then "Foo.Flag is now true"
            else "Foo.Flag is now false"));
   New_Line;

   Put_Line("Pushing an 'Example_Child' value (Counter => 5, Flag => False) to the stack.");
   Userdata_Package.Push_Class_Wide(L, Example_Child_Object'Unchecked_Access);
   Print_Stack(L);
   Put_Line("Saving userdata in global 'bar'");
   L.SetGlobal("bar");
   New_Line;

   Put_Line("Calling 'foo:increment()' in Lua");
   Discard := L.LoadString("foo:increment()");
   L.Call(nargs => 0, nresults => 0);
   Put_Line("Calling 'foo:toggle()' (a class-wide operation) in Lua");
   Discard := L.LoadString("foo:toggle()");
   L.Call(nargs => 0, nresults => 0);
   Put_Line("Calling 'bar:toggle()' (a class-wide operation) in Lua");
   Discard := L.LoadString("bar:toggle()");
   L.Call(nargs => 0, nresults => 0);
   New_Line;

   Put_Line("Retrieving the value of 'Flag' and 'Counter' from foo & bar.");
   L.GetGlobal("foo");
   Result := Userdata_Package.ToUserdata(L, -1).Flag;
   Put_Line((if Result then "foo.Flag is now true"
            else "foo.Flag is now false"));
   Counter := Userdata_Package.ToUserdata(L, -1).Counter;
   Put_Line("foo.Counter = " & Integer'Image(Counter));
   L.GetGlobal("bar");
   Result := Userdata_Package.ToUserdata_Class_Wide(L, -1).Flag;
   Put_Line((if Result then "bar.Flag is now true"
            else "bar.Flag is now false"));
   Counter := Userdata_Package.ToUserdata_Class_Wide(L, -1).Counter;
   Put_Line("bar.Counter = " & Integer'Image(Counter));
   New_Line;

end Userdata_Example;
