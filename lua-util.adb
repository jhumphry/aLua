-- Lua.Util
-- Utility routines to go with the Ada 2012 Lua interface

-- Copyright (c) 2015, James Humphry - see LICENSE for terms

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Lua.Util is

   procedure Print_Stack(L : Lua_State'Class) is
   begin
      if L.GetTop = 0 then
         Put_Line("Stack now contains: -*- Stack is empty -*-");
         goto Exit_Point;
      end if;

      Put_Line("Stack now contains:");
      Put_Line("Rel index   : Abs index   : Type     : Contents");
      for I in reverse 1..L.GetTop loop
         Put(I - L.GetTop - 1); Set_Col(13); Put(": ");
         Put(L.AbsIndex(I)); Set_Col(27); Put(": ");
         Put(L.TypeName(L.TypeInfo(I))); Set_Col(38); Put(": ");
         case L.TypeInfo(I) is
            when TBOOLEAN =>
               Put((if L.ToBoolean(I) then "true" else "false"));
            when TNUMBER =>
               Put(L.ToNumber(I), Aft => 0, Exp => 0);
            when TSTRING =>
               Put("'" & L.ToString(I) & "'");
            when TUSERDATA =>
               declare
                  Name : constant String := L.Userdata_Name(I);
               begin
                  if Name = "" then
                     Put(" - Non-Ada userdata");
                  else
                     Put(Name);
                  end if;
               end;
            when others =>
               Put("-");
         end case;
         New_Line;
      end loop;
      <<Exit_Point>>
      null;
   end Print_Stack;

end Lua.Util;
