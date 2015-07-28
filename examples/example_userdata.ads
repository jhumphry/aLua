-- Example_Userdata
-- A simple example of an Ada type turned into a Lua userdata type

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Lua, Lua.Userdata;
use Lua;

package Example_Userdata is

   type Example is tagged
      record
         Number : Integer;
         Flag : Boolean;
      end record;

   package Userdata_Package is new Lua.Userdata(T => Example);

   function Toggle (L : Lua_State'Class) return Natural;

   procedure Register_Operations(L : Lua_State'Class);

end Example_Userdata;
