-- Example_Userdata
-- A simple example of an Ada type turned into a Lua userdata type

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO;

package body Example_Userdata is

   function Increment (L : Lua_State'Class) return Natural is
      Object : constant access Child
        := Child_Userdata.ToUserdata(L, -1);
   begin
      Ada.Text_IO.Put_Line(" - Now incrementing the Child userdata object's counter in an Ada function called from Lua -");
      Object.Counter := Object.Counter + 1;
      return 0;
   end Increment;

   function Toggle (L : Lua_State'Class) return Natural is
      Object : constant access Grandparent'Class
        := Grandparent_Userdata.ToUserdata_Class(L, -1);
   begin
      Ada.Text_IO.Put_Line(" - Now toggling the Grandparent'Class userdata object's flag in an Ada function called from Lua -");
      Object.Flag := not Object.Flag;
      return 0;
   end Toggle;

   procedure Register_Operations(L : Lua_State'Class) is
   begin
      Grandparent_Userdata.NewMetaTable(L);
      Grandparent_Userdata.AddOperation(L, "toggle", Toggle'Access);
      L.Pop(L.GetTop);
      Child_Userdata.NewMetaTable(L);
      Child_Userdata.AddOperation(L, "toggle", Toggle'Access);
      Child_Userdata.AddOperation(L, "increment", Increment'Access);
      L.Pop(L.GetTop);
   end Register_Operations;

end Example_Userdata;
