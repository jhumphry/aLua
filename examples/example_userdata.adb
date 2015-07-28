-- Example_Userdata

-- Copyright (c) 2015, James Humphry

with Ada.Text_IO;

package body Example_Userdata is

   function Toggle (L : State'Class) return Natural is
      Object : access Example := Userdata_Package.ToUserdata(L, -1);
   begin
      Ada.Text_IO.Put_Line(" - Now toggling the object's flag from Lua -");
      Object.Flag := not Object.Flag;
      return 0;
   end Toggle;

   procedure Register_Operations(L : State'Class) is
   begin
      Userdata_Package.NewMetaTable(L);
      Userdata_Package.AddOperation(L, "toggle", Toggle'Access);
      L.Pop(L.GetTop);
   end Register_Operations;


end Example_Userdata;
