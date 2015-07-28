-- Lua.Userdata

-- Adding Ada objects to Lua environment

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

private with Ada.Tags;
private with Lua.Internal;

generic
   type T is tagged private;
package Lua.Userdata is

   procedure Push (L : in State'Class; D : not null access T);
   function ToUserdata (L : in State'Class; index : in Integer)
                        return not null access T;
   procedure NewMetaTable (L : in State'Class);
   function GetMetaTable (L : in State'Class) return Boolean;
   procedure GetMetaTable (L : in State'Class);
   procedure AddOperation (L : in State'Class; Name : in String; Op : AdaFunction);

private
   type Ada_Userdata is
      record
         Tag : Ada.Tags.Tag := T'Tag;
         Data : not null access T;
      end record;

end Lua.Userdata;
