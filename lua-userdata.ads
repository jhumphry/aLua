-- Lua.Userdata
-- Adding Ada objects of a specified tagged type to the Lua environment

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

generic
   type T is tagged private;
package Lua.Userdata is

   type Access_Userdata is not null access all T;
   type Access_Userdata_Class is not null access all T'Class;

   -- Push an access-to-T to the stack of Lua state L as a UserData value. If a
   -- metatable has been registered for the type then it is associated with the
   -- new UserData value.
   procedure Push (L : in Lua_State'Class; D : not null access T);

   -- Push an access-to-T'Class to the stack of Lua state L as a UserData value.
   -- If a metatable has been registered for the type then it is associated with
   -- the new UserData value.
   procedure Push_Class (L : in Lua_State'Class; D : not null access T'Class);

   -- Check that the value at the index given is a valid Userdata that
   -- is convertible to an Ada access-to-T or access-to-T'Class.
   function IsAdaUserdata (L : in Lua_State'Class; index : in Integer)
                           return Boolean;

   -- Retrieve an access-to-T from the stack of Lua state L at the given index.
   -- Checks that the value indicated is a non-class-wide userdata value, and
   -- that the tag is correct.
   function ToUserdata (L : in Lua_State'Class; index : in Integer)
                        return Access_Userdata;

   -- Retrieve an access-to-T from the stack of Lua state L at the given index.
   -- Checks that the value indicated is a userdata value, and that the tag is
   -- correct. If the value is an access-to-S, where S is in T'Class, it will be
   -- converted to an access-to-T'Class.
   function ToUserdata_Class (L : in Lua_State'Class; index : in Integer)
                        return Access_Userdata_Class;

   -- Create a new metatable for this type. It will be stored in the registry
   -- under 'Ada_UserData:ET' where ET is the result of T'External_Tag. If
   -- Set_Indexable is true then the key '__index' is set to the table itself,
   -- so writing 'a:foo()' in Lua will cause a call of metatable['foo'](a).
   procedure NewMetaTable (L : in Lua_State'Class;
                           Set_Indexable : Boolean := True);

   -- Get the metatable for the type and push it to the stack.
   -- Returns true if successful, false otherwise
   function GetMetaTable (L : in Lua_State'Class) return Boolean;

   -- Get the metatable for the type and push it to the stack. If unsuccessful,
   -- raises Program_Error.
   procedure GetMetaTable (L : in Lua_State'Class);

   -- Registers a function Op under the given name. The top value on the stack
   -- should be the metatable for the type.
   procedure AddOperation (L : in Lua_State'Class;
                           Name : in String;
                           Op : AdaFunction);
end Lua.Userdata;
