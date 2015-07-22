-- Lua.Lib

-- Supporting the Lua standard libraries

-- Copyright (c) 2015, James Humphry

-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Finalization;

private with Interfaces.C.Strings;

package Lua.Libs is

   type Lua_Standard_Library is (Base_Lib,
                                 Package_Lib,
                                 Coroutine_Lib,
                                 String_Lib,
                                 UTF8_Lib,
                                 Table_Lib,
                                 Math_Lib,
                                 IO_Lib,
                                 OS_Lib,
                                 Debug_Lib,
                                 Bit32_Lib);

   procedure OpenLibs (L : in State);

   procedure Require_Standard_Library (L : in State;
                                       Library : in Lua_Standard_Library;
                                       Set_Global : in Boolean := True);

end Lua.Libs;
