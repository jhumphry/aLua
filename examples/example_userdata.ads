-- Example_Userdata
-- A simple example of an Ada type turned into a Lua userdata type

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Lua, Lua.Userdata;
use Lua;

package Example_Userdata is

   type Grandparent is abstract tagged
      record
         Flag : Boolean;
      end record;

   type Parent is new Grandparent with null record;

   type Child is new Parent with
      record
         Counter : Integer := 0;
      end record;

   package Grandparent_Userdata is new Lua.Userdata(T => Grandparent);
   package Child_Userdata is new Lua.Userdata(T => Child);

   function Toggle (L : Lua_State'Class) return Natural;
   function Increment (L : Lua_State'Class) return Natural;

   procedure Register_Operations(L : Lua_State'Class);

end Example_Userdata;
