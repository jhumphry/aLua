-- Userdata_Example
-- A example of using the Ada values in Lua via UserData values

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

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

   Parent_Object : aliased Parent := (Flag => True);
   Child_Object : aliased Child := (Counter => 3, Flag => False);

begin

   Put_Line("Using Ada values as userdata in Lua");

   Put("Lua version: ");
   Put(Item => L.Version, Aft => 0, Exp => 0);
   New_Line;New_Line;

   Put_Line("There are three Ada types, Grandparent(abstract)->Parent->Child");
   Put_Line("Grandparent and descendants have a flag that can be toggled");
   Put_Line("Child has a counter that can be incremented");
   Put_Line("There are only two userdata types in Lua, Grandparent and Child.");
   New_Line;

   -- Register the "toggle" and "increment" operations in the metatables.
   Register_Operations(L);

   Put_Line("Pushing an 'Parent' value (Flag => True) to the stack as a Grandparent'Class userdata.");
   Grandparent_Userdata.Push_Class(L, Parent_Object'Unchecked_Access);
   Put_Line("Saving a copy of the userdata in global variable 'parent_foo'");
   L.PushValue(-1);
   L.SetGlobal("parent_foo");
   Print_Stack(L);
   Put_Line("Retrieving the value of 'Flag' from the value at the top of the stack.");
   Result := Grandparent_Userdata.ToUserdata(L, -1).Flag;
   Put_Line((if Result then "parent_foo.Flag is now true"
            else "parent_foo.Flag is now false"));
   New_Line;

   Put_Line("Pushing an 'Child' value (Counter => 3, Flag => False) to the stack as a Child userdata.");
   Child_Userdata.Push(L, Child_Object'Unchecked_Access);
   Print_Stack(L);
   Put_Line("Saving the userdata at the top of the stack in global variable 'child_bar'");
   L.SetGlobal("child_bar");
   New_Line;

   Put_Line("Clearing the stack");
   L.SetTop(0);
   New_Line;

   Put_Line("Calling 'parent_foo:toggle()' in Lua");
   Discard := L.LoadString_By_Copy("parent_foo:toggle()");
   L.Call(nargs => 0, nresults => 0);
   New_Line;
   Put_Line("Calling 'child_bar:toggle()' in Lua");
   Discard := L.LoadString_By_Copy("child_bar:toggle()");
   L.Call(nargs => 0, nresults => 0);
   New_Line;
   Put_Line("Calling 'child_bar:increment()' in Lua");
   Discard := L.LoadString_By_Copy("child_bar:increment()");
   L.Call(nargs => 0, nresults => 0);
   New_Line;

   New_Line;

   Put_Line("Retrieving the value of 'Flag' from parent_foo and child_bar, " &
              "treating them as Grandparent values.");
   L.GetGlobal("parent_foo");
   Result := Grandparent_Userdata.ToUserdata(L, -1).Flag;
   Put_Line((if Result then "parent_foo.Flag is now true"
            else "parent_foo.Flag is now false"));
   L.SetTop(0);
   L.GetGlobal("child_bar");
   Result := Grandparent_Userdata.ToUserdata(L, -1).Flag;
   Put_Line((if Result then "child_bar.Flag is now true"
            else "child_bar.Flag is now false"));

   Put_Line("Retrieving the value of 'Counter' from child_bar, " &
              "treating it as a Child value.");
   Counter := Child_Userdata.ToUserdata(L, -1).Counter;
   Put_Line("child_bar.Counter = " & Integer'Image(Counter));
   New_Line;

end Userdata_Example;
