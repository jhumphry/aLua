-- Lua

-- An Ada 2012 interface to the Lua language

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
