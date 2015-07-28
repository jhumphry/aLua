-- Example_Adafunctions
-- A example of using the Ada 2012 interface to Lua for functions / closures etc

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Text_IO; use Ada.Text_IO;

package body Example_AdaFunctions is

   function FooBar (L : Lua_State'Class) return Natural is
      x : Long_Float := L.ToNumber(-1);
   begin
      Put_Line("* In Ada function foobar(" & Long_Float'Image(x) & ")");
      L.pop(1);
      L.PushNumber(x*x);
      return 1;
   end FooBar;

   function Multret (L : Lua_State'Class) return Natural is
      X : Long_Long_Integer := L.ToInteger(-1);
   begin
      Put_Line("* In Ada function multret(" & Long_Long_Integer'Image(X) & ")");
      L.pop(1);
      for I in reverse Long_Long_Integer range 1..X loop
         L.PushInteger(I);
      end loop;
      return Integer(X);
   end Multret;

   function Closure (L : Lua_State'Class) return Natural is
      p : Long_Float := L.ToNumber(UpvalueIndex(1));
      x : Long_Float := L.ToNumber(-1);
   begin
      Put_Line("* In Ada closure foobar" &
                 "<" & Long_Float'Image(p) & ">" &
                 "(" & Long_Float'Image(x) & ")");
      L.pop(1);
      L.PushNumber(x*p);
      return 1;
   end Closure;

end Example_AdaFunctions;
