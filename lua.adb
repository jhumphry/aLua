-- Lua

-- An Ada 2012 interface to the Lua language

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int;
with Interfaces.C.Strings;

with Lua.Internal, Lua.AuxInternal;

package body Lua is

   -------------
   -- Version --
   -------------

   function Version (L : State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

   ------------
   -- Status --
   ------------

   function Status (L : State) return Thread_Status is
      (Thread_Status'Val(Internal.lua_status(L.L)));

   ----------
   -- Push --
   ----------

   procedure Push (L : in out State; n : in Number) is
   begin
      Internal.lua_pushnumber(L.L, Internal.lua_Number(n));
   end Push;

   ---------------
   -- ToNumber --
   ---------------

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

   -----------
   -- Arith --
   -----------

   procedure Arith (L : in out State; op : in Arith_Op) is
   begin
      Internal.lua_arith(L.L, Arith_Op'Pos(op));
   end Arith;

   ---
   --- *** Stack manipulation and information
   ---

   function AbsIndex (L : in State; idx : in Integer) return Integer is
     (Integer(Internal.lua_absindex(L.L, C.int(idx))));

   function CheckStack (L : in State; n : in Integer) return Boolean is
     (Internal.lua_checkstack(L.L, C.int(n)) /= 0);

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

   ---
   --- *** Type information
   ---

   function TypeInfo (L : in State; index : in Integer) return Lua_Type is
     (
      Lua_Type'Val(Internal.lua_type(L.L, C.int(index)))
     );

   function TypeName (L : in State; tp : in Lua_Type) return String is
     (
      C.Strings.Value(Internal.lua_typename(L.L, C.int(Lua_Type'Pos(tp))))
     );

   --
   -- *** Resource Management ***
   --

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out State) is
   begin
      Object.L := AuxInternal.luaL_newstate;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out State) is
   begin
      Internal.lua_close(Object.L);
   end Finalize;

end Lua;
