-- Lua.Userdata
-- Adding Ada objects to Lua environment

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

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
