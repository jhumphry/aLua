-- Lua

-- An Ada 2012 interface to the Lua language

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int;

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
