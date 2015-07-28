-- Lua.Userdata
-- Adding Ada objects to Lua environment

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

private with Ada.Tags;
private with Lua.Internal;

generic
   type T is tagged private;
package Lua.Userdata is

   -- Push an access-to-T to the stack of Lua state L as a UserData value. If a
   -- metatable has been registered for the type then it is associated with the
   -- new UserData value.
   procedure Push (L : in State'Class; D : not null access T);

   -- Retrieve an access-to-T from the stack of Lua state L at the given index.
   -- Checks that the value indicated is a userdata value, and that the tag
   -- is correct.
   function ToUserdata (L : in State'Class; index : in Integer)
                        return not null access T;

   -- Create a new metatable for this type. It will be stored in the registry
   -- under 'Ada_UserData:ET' where ET is the result of T'External_Tag. By
   -- default the key '__index' is set to the table itself, so a:foo() will
   -- cause a call of metatable['foo'](a).
   procedure NewMetaTable (L : in State'Class);

   -- Get the metatable for the type and push it to the stack.
   -- Returns true if successful, false otherwise
   function GetMetaTable (L : in State'Class) return Boolean;

   -- Get the metatable for the type and push it to the stack. If unsuccessful,
   -- raises Program_Error.
   procedure GetMetaTable (L : in State'Class);

   -- Registers a function Op under the given name. The top value on the stack
   -- should be the metatable for the type.
   procedure AddOperation (L : in State'Class;
                           Name : in String;
                           Op : AdaFunction);

private
   type Ada_Userdata is
      record
         Tag : Ada.Tags.Tag := T'Tag;
         Data : not null access T;
      end record;

end Lua.Userdata;
