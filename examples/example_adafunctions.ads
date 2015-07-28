-- Example_Adafunctions
-- A example of using the Ada 2012 interface to Lua for functions / closures etc

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Lua; use Lua;

package Example_AdaFunctions is

   function FooBar (L : State'Class) return Natural;

   function Multret (L : State'Class) return Natural;

   function Closure (L : State'Class) return Natural;

end Example_AdaFunctions;
