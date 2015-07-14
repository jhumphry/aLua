-- Lua

-- An Ada 2012 interface to the Lua language

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int;
with Interfaces.C.Strings;

with Lua.Internal, Lua.AuxInternal;

package body Lua is

   --
   -- *** Special stack positions and the registry
   --

   function UpvalueIndex (i : in Integer) return Integer is
      (RegistryIndex - i);

   --
   -- *** Basic state control
   --

   function Version (L : State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

   function Status (L : State) return Thread_Status is
      (Thread_Status'Val(Internal.lua_status(L.L)));

   --
   -- *** Operations on values
   --

   procedure Push (L : in out State; n : in Number) is
   begin
      Internal.lua_pushnumber(L.L, Internal.lua_Number(n));
   end Push;

   function ToNumber (L : in State; index : in Integer) return Number is
      isnum : aliased C.int := 0;
      result : Internal.lua_Number;
   begin
      result := Internal.lua_tonumberx(L.L , C.int(index), isnum'Access);
      if isnum = 0 then
         raise Constraint_Error with "Value at Lua stack index "
           & Integer'Image(index)
           & " not a number.";
      end if;
      return Number(result);
   end ToNumber;

   procedure Arith (L : in out State; op : in Arith_Op) is
   begin
      Internal.lua_arith(L.L, Arith_Op'Pos(op));
   end Arith;

   function Compare (L : in State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean is
     (Internal.lua_compare(L.L,
                           C.int(index1),
                           C.int(index2),
                           C.int(Comparison_Op'Pos(op))) = 1);

   --
   -- *** Garbage Collector control
   ---

   procedure GC (L : in State; what : in GC_Op) is
      Discard : C.int;
   begin
      Discard := Internal.lua_gc(L.L, C.int(GC_Op'Pos(what)), 0);
   end GC;

   function GC (L : in State; what : in GC_Param; data : in Integer)
                return Integer is
      (Integer(Internal.lua_gc(L.L, C.int(GC_Param'Pos(what)), C.int(data))));

   function GC (L : in State) return Boolean is
       (Internal.lua_gc(L.L, GCISRUNNING, 0) /= 0);

   --
   -- *** Stack manipulation and information
   --

   function AbsIndex (L : in State; idx : in Integer) return Integer is
     (Integer(Internal.lua_absindex(L.L, C.int(idx))));

   function CheckStack (L : in State; n : in Integer) return Boolean is
     (Internal.lua_checkstack(L.L, C.int(n)) /= 0);

   procedure Copy (L : in out State; fromidx : in Integer; toidx : in Integer) is
   begin
      Internal.lua_copy(L.L, C.int(fromidx), C.int(toidx));
   end Copy;

   function GetTop (L : in State) return Integer is
     (Integer(Internal.lua_gettop(L.L)));

   procedure Insert (L : in out State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), 1);
   end Insert;

   procedure Pop (L : in out State; n : in Integer) is
   begin
      Internal.lua_settop(L.L, -C.int(n)-1);
   end Pop;

   procedure PushValue (L : in out State; index : in Integer) is
   begin
      Internal.lua_pushvalue(L.L, C.int(index));
   end PushValue;

   procedure Remove (L : in out State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), -1);
      Internal.lua_settop(L.L, -2);
   end Remove;

   procedure Replace (L : in out State; index : in Integer) is
   begin
      Internal.lua_copy(L.L, -1, C.int(index));
      Internal.lua_settop(L.L, -2);
   end Replace;

   procedure Rotate (L : in out State; idx : in Integer; n : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(idx), C.int(n));
   end Rotate;

   procedure SetTop (L : in out State; index : in Integer) is
   begin
      Internal.lua_settop(L.L, C.int(index));
   end SetTop;

   --
   -- *** Type information
   --

   function TypeInfo (L : in State; index : in Integer) return Lua_Type is
     (
      Lua_Type'Val(Internal.lua_type(L.L, C.int(index)))
     );

   function TypeName (L : in State; tp : in Lua_Type) return String is
     (
      C.Strings.Value(Internal.lua_typename(L.L, C.int(Lua_Type'Pos(tp))))
     );

   --
   -- *** Table Manipulation
   --

   procedure createtable (L : in out State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0) is
   begin
      Internal.lua_createtable(L.L, C.int(narr), C.int(nrec));
   end createtable;

   procedure newtable (L : in out State) is
   begin
      Internal.lua_createtable(L.L, 0, 0);
   end newtable;

   function getfield (L : in out State; index : in Integer; k : in String)
                      return Lua_Type is
     (
      Lua_Type'Val(
                   Internal.lua_getfield(L.L,
                                         C.int(index),
                                         C.Strings.New_String(k & ASCII.Nul))
                  )
     );

   procedure getfield (L : in out State; index : in Integer; k : in String) is
      Discard : C.int;
   begin
      Discard := Internal.lua_getfield(L.L,
                                       C.int(index),
                                       C.Strings.New_String(k));
   end getfield;

   function geti (L : in out State; index : in Integer; i : in Integer)
                  return Lua_Type is
     (
      Lua_Type'Val(Internal.lua_geti(L.L, C.int(index), Long_Long_Integer(i)))
     );

   procedure geti (L : in out State; index : in Integer; i : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_geti(L.L, C.int(index), Long_Long_Integer(i));
   end geti;

   function gettable (L : in out State; index : in Integer) return Lua_Type is
     (
      Lua_Type'Val(Internal.lua_gettable(L.L, C.int(index)))
     );

   procedure gettable (L : in out State; index : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_gettable(L.L, C.int(index));
   end gettable;

   function next (L : in out State; index : in Integer) return Boolean is
     (Internal.lua_next(L.L, C.int(index)) /= 0);

   procedure setfield (L : in out State; index : in Integer; k : in String) is
   begin
      Internal.lua_setfield(L.L, C.int(index), C.Strings.New_String(k));
   end setfield;

   procedure seti (L : in out State; index : in Integer; i : in Integer) is
   begin
      Internal.lua_seti(L.L, C.int(index), Long_Long_Integer(i));
   end seti;

   procedure settable (L : in State; index : in Integer) is
   begin
      Internal.lua_settable(L.L, C.int(index));
   end settable;

   --
   -- *** Globals and Metatables
   --

   function getglobal (L : in out State; name : in String) return Lua_Type is
     (Lua_Type'Val(Internal.lua_getglobal(L.L, C.Strings.New_String(name))));

   procedure getglobal (L : in out State; name : in String) is
      Discard : C.int;
   begin
      Discard := Internal.lua_getglobal(L.L, C.Strings.New_String(name));
   end getglobal;

   function getmetatable (L : in out State; index : in Integer) return Boolean is
     (Internal.lua_getmetatable(L.L, C.int(index)) /= 0);

   procedure getmetatable (L : in out State; index : in Integer) is
   begin
      if Internal.lua_getmetatable(L.L, C.int(index)) = 0 then
         raise Lua_Error with "No metatable exists for stack index: " &
           Integer'Image(index);
      end if;
   end getmetatable;

   procedure setglobal (L : in out State; name : in String) is
   begin
      Internal.lua_setglobal(L.L, C.Strings.New_String(name));
   end setglobal;

   procedure setmetatable (L : in out State; index : in Integer) is
      Discard : C.int;
   begin
      -- According to the Lua documentation, lua_setmetatable does not have a
      -- return value, but according to lua.h it does...
      Discard := Internal.lua_setmetatable(L.L, C.int(index));
   end setmetatable;

   --
   -- *** Resource Management ***
   --

   procedure Initialize (Object : in out State) is
   begin
      Object.L := AuxInternal.luaL_newstate;
   end Initialize;

   procedure Finalize (Object : in out State) is
   begin
      Internal.lua_close(Object.L);
   end Finalize;

end Lua;
