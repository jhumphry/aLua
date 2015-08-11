-- Lua.Userdata
-- Adding Ada objects of a specified tagged type to the Lua environment

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Ada.Tags;

with System.Address_To_Access_Conversions;

with Interfaces, Interfaces.C, Interfaces.C.Strings;
use Interfaces;

with Lua.Internal, Lua.AuxInternal;

package body Lua.Userdata is

   use type Ada.Tags.Tag;
   use type C.int;
   use type C.size_t;

   --
   -- *** Types used internally
   --

   -- It might seem more obvious to make Class_Wide a discriminant and to use a
   -- variant record, but this would mean passing an indefinite type to/from C,
   -- and would require changing the discriminant on new objects allocated by
   -- Lua. As this type is only used in the package body, it is more practical
   -- to use an approach that is less idiomatic in Ada by including both types
   -- of pointers in every record and remembering to check each time before use.
   type Ada_Userdata is
      record
         Class_Wide : Boolean;
         Tag : Ada.Tags.Tag := T'Tag;
         Data : access T;
         Data_Class_Wide : access T'Class;
      end record;

   --
   -- *** Conversions between C types and Ada types
   --

   package Address_To_Ada_Userdata is
     new System.Address_To_Access_Conversions(Object => Ada_Userdata);

   procedure Push (L : in Lua_State'Class; D : not null access T) is
      UserData_Address : constant System.Address
        := Internal.lua_newuserdata(L.L, (Ada_Userdata'Size+7)/8);
      UserData_Access : constant access Ada_Userdata
        := Address_To_Ada_Userdata.To_Pointer(UserData_Address);
      Has_Metatable : Boolean;
   begin
      UserData_Access.all := Ada_Userdata'(Class_Wide => False,
                                           Tag => T'Tag,
                                           Data => D,
                                           Data_Class_Wide => null);
      Has_Metatable := GetMetaTable(L);
      if Has_Metatable then
         SetMetatable(L, -2);
      end if;
   end Push;

   procedure Push_Class (L : in Lua_State'Class; D : not null access T'Class) is
      UserData_Address : constant System.Address
        := Internal.lua_newuserdata(L.L, (Ada_Userdata'Size+7)/8);
      UserData_Access : constant access Ada_Userdata
        := Address_To_Ada_Userdata.To_Pointer(UserData_Address);
      Has_Metatable : Boolean;
   begin
      UserData_Access.all := Ada_Userdata'(Class_Wide => True,
                                           Tag => T'Tag,
                                           Data => null,
                                           Data_Class_Wide => D);
      Has_Metatable := GetMetaTable(L);
      if Has_Metatable then
         SetMetatable(L, -2);
      end if;
   end Push_Class;

   function IsAdaUserdata (L : in Lua_State'Class; index : in Integer)
                           return Boolean is
      UserData_Address : constant System.Address
        := Internal.lua_touserdata(L.L, C.int(index));
      UserData_Access : constant access Ada_Userdata
        := Address_To_Ada_Userdata.To_Pointer(UserData_Address);
   begin
      if UserData_Access = null then
         return False;
      elsif not Ada.Tags.Is_Descendant_At_Same_Level(UserData_Access.Tag, T'Tag) then
         return False;
      end if;
      return True;
   end IsAdaUserdata;

   function ToUserdata (L : in Lua_State'Class; index : in Integer)
                        return Access_Userdata
   is
      UserData_Address : constant System.Address
        := Internal.lua_touserdata(L.L, C.int(index));
      UserData_Access : constant access Ada_Userdata
        := Address_To_Ada_Userdata.To_Pointer(UserData_Address);
   begin
      if UserData_Access = null then
         raise Lua_Error with "Attempting to access non-userdata as userdata";
      elsif not Ada.Tags.Is_Descendant_At_Same_Level(UserData_Access.Tag, T'Tag) then
         raise Lua_Error with "Attempting invalid userdata type conversion: " &
           Ada.Tags.External_Tag(UserData_Access.Tag) &
           (if UserData_Access.Class_Wide then "'Class" else "") &
           " to " & T'External_Tag;
      end if;

      -- Note that because we did an unchecked conversion, Ada already assumes
      -- that Data/Data_Class_Wide is an access-to-T/T'Class type, so this
      -- conversion might not be checked thoroughly enough. However, as we have
      -- done an explicit check that the actual target of Data/Data_Class_Wide
      -- is covered by T and accessable at the same level this should be safe
      -- enough.
      if UserData_Access.Class_Wide then
         return Access_Userdata(UserData_Access.Data_Class_Wide);
      else
         return Access_Userdata(UserData_Access.Data);
      end if;

   end ToUserdata;

   function ToUserdata_Class (L : in Lua_State'Class; index : in Integer)
                        return Access_Userdata_Class
   is
      UserData_Address : constant System.Address
        := Internal.lua_touserdata(L.L, C.int(index));
      UserData_Access : constant access Ada_Userdata
        := Address_To_Ada_Userdata.To_Pointer(UserData_Address);
   begin
      if UserData_Access = null then
         raise Lua_Error with "Attempting to access non-userdata as userdata";
      elsif not Ada.Tags.Is_Descendant_At_Same_Level(UserData_Access.Tag, T'Tag) then
         raise Lua_Error with "Attempting invalid userdata type conversion: " &
           Ada.Tags.External_Tag(UserData_Access.Tag) &
           (if UserData_Access.Class_Wide then "'Class" else "") &
           " to " & T'External_Tag & "'Class";
      end if;

      -- Note that because we did an unchecked conversion, Ada already assumes
      -- that Data/Data_Class_Wide is an access-to-T/T'Class type, so this
      -- conversion might not be checked thoroughly enough. However, as we have
      -- done an explicit check that the actual target of Data/Data_Class_Wide
      -- is covered by T and accessable at the same level this should be safe
      -- enough.
      if UserData_Access.Class_Wide then
         return Access_Userdata_Class(UserData_Access.Data_Class_Wide);
      else
         return Access_Userdata_Class(UserData_Access.Data);
      end if;
   end ToUserdata_Class;

   procedure NewMetaTable (L : in Lua_State'Class;
                           Set_Indexable : Boolean := True) is
      tname : C.Strings.chars_ptr
        := C.Strings.New_String("Ada:" & T'External_Tag);
      Result : C.int;
   begin
      Result := AuxInternal.luaL_newmetatable(L.L, tname);
      C.Strings.Free(tname);
      if Result = 0 then
         raise Lua_Error with "Metatable could not be created or already exists";
      elsif Set_Indexable then
         -- Setting the metatables __index field to itself so any unidentified
         -- operations of the form object:operation() will be looked up in the
         -- metatable itself
         L.PushValue(-1);
         L.SetField(-2, "__index");
      end if;
   end NewMetaTable;

   function GetMetaTable (L : in Lua_State'Class) return Boolean is
       tname : C.Strings.chars_ptr
        := C.Strings.New_String("Ada:" & T'External_Tag);
      Result : C.int;
   begin
      Result := Internal.lua_getfield(L.L, C.int(RegistryIndex), tname);
      C.Strings.Free(tname);
      return Result /= 0;
   end GetMetaTable;

   procedure GetMetaTable (L : in Lua_State'Class) is
      Success : constant Boolean := GetMetaTable(L);
   begin
      if not Success then
         raise Lua_Error with "No metatable exists for this type";
      end if;
   end GetMetaTable;

   procedure AddOperation (L : in Lua_State'Class;
                           Name : in String;
                           Op : AdaFunction) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(Name);
   begin
      L.PushAdaClosure(Op, 0);
      Internal.lua_setfield(L.L, -2, C_name);
      C.Strings.Free(C_name);
   end AddOperation;

end Lua.Userdata;
