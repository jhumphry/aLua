-- Example_Adafunctions

-- A example of using the Ada 2012 interface to Lua for functions / closures etc

-- Copyright (c) 2015, James Humphry

-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Text_IO; use Ada.Text_IO;

package body Example_AdaFunctions is

   function FooBar (L : State'Class) return Natural is
      x : Long_Float := L.ToNumber(-1);
   begin
      Put_Line("* In Ada function foobar(" & Long_Float'Image(x) & ")");
      L.pop(1);
      L.PushNumber(x*x);
      return 1;
   end FooBar;

   function Multret (L : State'Class) return Natural is
      X : Long_Long_Integer := L.ToInteger(-1);
   begin
      Put_Line("* In Ada function multret(" & Long_Long_Integer'Image(X) & ")");
      L.pop(1);
      for I in reverse Long_Long_Integer range 1..X loop
         L.PushInteger(I);
      end loop;
      return Integer(X);
   end Multret;

   function Closure (L : State'Class) return Natural is
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
