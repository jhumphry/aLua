-- Example_Adafunctions
-- A example of using the Ada 2012 interface to Lua for functions / closures etc

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Lua; use Lua;

package Example_AdaFunctions is

   function FooBar (L : Lua_State'Class) return Natural;

   function Multret (L : Lua_State'Class) return Natural;

   function Closure (L : Lua_State'Class) return Natural;

end Example_AdaFunctions;
