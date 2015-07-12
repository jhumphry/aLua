-- Lua

-- An Ada 2012 interface to the Lua language

with Lua.Internal, Lua.AuxInternal;

package body Lua is

   -------------
   -- Version --
   -------------

   function Version (L : State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

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
