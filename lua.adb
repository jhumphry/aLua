-- Lua

-- An Ada 2012 interface to the Lua language

with Ada.Unchecked_Conversion, Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int, Interfaces.C.size_t;
with Interfaces.C.Strings;

with System;
use type System.Address;

with Lua.Internal, Lua.AuxInternal, Lua.LibInternal;
with Ada.Finalization;

package body Lua is

   --
   -- *** Conversions between Ada enumerations and C integer constants
   --

   function Int_To_Thread_Status is new
     Ada.Unchecked_Conversion(Source => C.int,
                              Target => Thread_Status);

   function Arith_Op_To_Int is new Ada.Unchecked_Conversion(Source => Arith_Op,
                                                            Target => C.int);

   function Comparison_Op_To_Int is new
     Ada.Unchecked_Conversion(Source => Comparison_Op,
                              Target => C.int);

   function GC_Inputs_To_Int is new
     Ada.Unchecked_Conversion(Source => GC_Inputs,
                              Target => C.int);

   function Lua_Type_To_Int is new Ada.Unchecked_Conversion(Source => Lua_Type,
                                                            Target => C.Int);

   function Int_To_Lua_Type is new Ada.Unchecked_Conversion(Source => C.Int,
                                                            Target => Lua_Type);

   --
   -- *** Conversions between C pointers and Ada types
   --

   function AdaFunction_To_Address is new
     Ada.Unchecked_Conversion(Source => AdaFunction,
                              Target => System.Address);


   function Address_To_AdaFunction is new
     Ada.Unchecked_Conversion(Source => System.Address,
                              Target => AdaFunction);
   --
   -- *** Special stack positions and the registry
   --

   function UpvalueIndex (i : in Integer) return Integer is
     (RegistryIndex - i - 1);
   -- Lua cannot call Ada functions directly, so a trampoline is used.
   -- The first UpvalueIndex is reserved for the address of the Ada function
   -- to be called by CFunction_Trampoline

   --
   -- *** Basic state control
   --

   function Version (L : State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

   function Status (L : State) return Thread_Status is
      (Int_To_Thread_Status(Internal.lua_status(L.L)));

   function LoadString (L : in State;
                        S : in String)
                        return Thread_Status is
      CS : C.Strings.chars_ptr;
      Result : C.int;
   begin
      CS := C.Strings.New_String(S);
      Result := AuxInternal.luaL_loadstring(L.L, C.Strings.New_String(S));
      C.Strings.Free(CS);
      return Int_To_Thread_Status(Result);
   end LoadString;

   --
   -- *** Calling, yielding and functions
   --

   procedure Call (L : in State;
                   nargs : in Integer;
                   nresults : in Integer) is
   begin
      Internal.lua_callk(L.L,
                         C.int(nargs),
                         C.int(nresults),
                         0,
                         null);
   end Call;

   function PCall (L : in State;
                   nargs : in Integer;
                   nresults : in Integer;
                   msgh : in Integer := 0)
                   return Thread_Status is
     (
      Int_To_Thread_Status(
                           Internal.lua_pcallk(L.L,
                                               C.int(nargs),
                                               C.int(nresults),
                                               C.int(msgh),
                                               0,
                                               null
                                              )

                          )
     );

   procedure Register(L : in State; name : in String; f : in AdaFunction) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
   begin
      PushAdaClosure(L, f, 0);
      Internal.lua_setglobal(L.L, C_name);
      C.Strings.Free(C_name);
   end Register;

   --
   -- *** Pushing values to the stack
   --

   procedure PushAdaClosure (L : in State;
                             f : in AdaFunction;
                             n : in Natural) is
   begin
      Internal.lua_pushlightuserdata(L.L, AdaFunction_To_Address(f));
      L.Rotate(L.GetTop - n, 1);
      Internal.lua_pushcclosure(L.L, CFunction_Trampoline'Access, C.int(1 + n));
   end PushAdaClosure;

   procedure PushAdaFunction (L : in State; f : in AdaFunction) is
   begin
      PushAdaClosure(L, f, 0);
   end PushAdaFunction;

   procedure PushBoolean (L : in  State; b : in Boolean) is
   begin
      Internal.lua_pushboolean(L.L, C.int( (if b then 1 else 0) ) );
   end PushBoolean;

   procedure PushInteger (L : in State; n : in Lua_Integer) is
   begin
      Internal.lua_pushinteger(L.L, Internal.lua_Integer(n));
   end PushInteger;

   procedure PushNil (L : in State) is
   begin
      Internal.lua_pushnil(L.L);
   end PushNil;

   procedure PushNumber (L : in  State; n : in Lua_Number) is
   begin
      Internal.lua_pushnumber(L.L, Internal.lua_Number(n));
   end PushNumber;

   procedure PushString (L : in State; s : in String) is
      C_s : C.Strings.chars_ptr;
      Discard : C.Strings.chars_ptr;
   begin
      C_s := C.Strings.New_String(s);
      Discard := Internal.lua_pushstring(L.L, C_s);
      C.Strings.Free(C_s);
   end PushString;

   function PushThread (L : in State) return Boolean is
     (Internal.lua_pushthread(L.L) = 1);

   procedure PushThread (L : in State) is
      Discard : C.int;
   begin
      Discard := Internal.lua_pushthread(L.L);
   end PushThread;

   function StringToNumber (L : in State; s : in String) return Boolean is
      C_String : C.Strings.chars_ptr := C.Strings.New_String(s);
      Result : C.size_t;
   begin
      Result := Internal.lua_stringtonumber(L.L, C_String);
      C.Strings.Free(C_String);
      return Result /= 0;
   end StringToNumber;

   --
   -- *** Pulling values from the stack
   --
   function ToBoolean (L : in State; index : in Integer) return Boolean is
      (Internal.lua_toboolean(L.L, C.int(Index)) /= 0);

   function ToInteger (L : in State; index : in Integer) return Lua_Integer is
      isnum : aliased C.int := 0;
      result : Internal.lua_Integer;
   begin
      result := Internal.lua_tointegerx(L.L , C.int(index), isnum'Access);
      if isnum = 0 then
         raise Constraint_Error with "Value at Lua stack index "
           & Integer'Image(index)
           & " is not convertible to an integer.";
      end if;
      return Lua_Integer(result);
   end ToInteger;

   function ToNumber (L : in State; index : in Integer) return Lua_Number is
      isnum : aliased C.int := 0;
      result : Internal.lua_Number;
   begin
      result := Internal.lua_tonumberx(L.L , C.int(index), isnum'Access);
      if isnum = 0 then
         raise Constraint_Error with "Value at Lua stack index "
           & Integer'Image(index)
           & " is not convertible to a number.";
      end if;
      return Lua_Number(result);
   end ToNumber;

   function ToString (L : in State; index : in Integer) return String is
      result : C.Strings.chars_ptr;
      len : aliased C.size_t := 0;
   begin
      result := Internal.lua_tolstring(L.L, C.int(index), len'Access);
      if len = 0 then
         return "";
      else
         declare
            converted_result : String(1..Integer(len+1));
         begin
            C.To_Ada(Item => C.Strings.Value(result),
                     Target => converted_result,
                     Count => Natural(len),
                     Trim_Nul => False);
            return converted_result;
         end;
      end if;
   end ToString;

   function ToThread (L : in State; index : in Integer) return Thread is
   begin
      return R : Thread do
         R.L := Internal.lua_tothread(L.L, C.int(index));
      end return;
   end ToThread;

   --
   -- *** Operations on values
   --

   procedure Arith (L : in State; op : in Arith_Op) is
   begin
      Internal.lua_arith(L.L, Arith_Op_To_Int(op));
   end Arith;

   function Compare (L : in State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean is
     (Internal.lua_compare(L.L,
                           C.int(index1),
                           C.int(index2),
                           Comparison_Op_To_Int(op)) = 1);

   procedure Len (L : in  State; index : Integer) is
   begin
      Internal.lua_len(L.L, C.int(index));
   end Len;

   function RawEqual(L : in State; index1, index2 : in Integer)
                     return Boolean is
      (Internal.lua_rawequal(L.L, C.int(index1), C.int(index2)) /= 0);

   function RawLen (L : in State; index : Integer) return Integer is
      (Integer(Internal.lua_rawlen(L.L, C.int(index))));

   --
   -- *** Garbage Collector control
   ---

   procedure GC (L : in State; what : in GC_Op) is
      Discard : C.int;
   begin
      Discard := Internal.lua_gc(L.L, GC_Inputs_To_Int(what), 0);
   end GC;

   function GC (L : in State; what : in GC_Param; data : in Integer)
                return Integer is
      (Integer(Internal.lua_gc(L.L, GC_Inputs_To_Int(what), C.int(data))));

   function GC (L : in State) return Boolean is
       (Internal.lua_gc(L.L, GC_Inputs_To_Int(GCISRUNNING), 0) /= 0);

   --
   -- *** Stack manipulation and information
   --

   function AbsIndex (L : in State; idx : in Integer) return Integer is
     (Integer(Internal.lua_absindex(L.L, C.int(idx))));

   function CheckStack (L : in State; n : in Integer) return Boolean is
     (Internal.lua_checkstack(L.L, C.int(n)) /= 0);

   procedure Copy (L : in State; fromidx : in Integer; toidx : in Integer) is
   begin
      Internal.lua_copy(L.L, C.int(fromidx), C.int(toidx));
   end Copy;

   function GetTop (L : in State) return Integer is
     (Integer(Internal.lua_gettop(L.L)));

   procedure Insert (L : in State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), 1);
   end Insert;

   procedure Pop (L : in State; n : in Integer) is
   begin
      Internal.lua_settop(L.L, -C.int(n)-1);
   end Pop;

   procedure PushValue (L : in State; index : in Integer) is
   begin
      Internal.lua_pushvalue(L.L, C.int(index));
   end PushValue;

   procedure Remove (L : in State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), -1);
      Internal.lua_settop(L.L, -2);
   end Remove;

   procedure Replace (L : in State; index : in Integer) is
   begin
      Internal.lua_copy(L.L, -1, C.int(index));
      Internal.lua_settop(L.L, -2);
   end Replace;

   procedure Rotate (L : in State; idx : in Integer; n : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(idx), C.int(n));
   end Rotate;

   procedure SetTop (L : in State; index : in Integer) is
   begin
      Internal.lua_settop(L.L, C.int(index));
   end SetTop;

   --
   -- *** Type information
   --

   function TypeInfo (L : in State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_type(L.L, C.int(index)))
     );

   function TypeName (L : in State; tp : in Lua_Type) return String is
     (
      C.Strings.Value(Internal.lua_typename(L.L, C.int(Lua_Type_To_Int(tp))))
     );

   --
   -- *** Table Manipulation
   --

   procedure createtable (L : in State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0) is
   begin
      Internal.lua_createtable(L.L, C.int(narr), C.int(nrec));
   end createtable;

   procedure newtable (L : in State) is
   begin
      Internal.lua_createtable(L.L, 0, 0);
   end newtable;

   function getfield (L : in State; index : in Integer; k : in String)
                      return Lua_Type is
      Result : C.int;
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Result := Internal.lua_getfield(L.L,
                                       C.int(index),
                                       C_k);
      C.Strings.Free(C_k);
      return Int_To_Lua_Type(Result);
   end getfield;

   procedure getfield (L : in State; index : in Integer; k : in String) is
      Discard : C.int;
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Discard := Internal.lua_getfield(L.L,
                                       C.int(index),
                                       C_k);
      C.Strings.Free(C_k);
   end getfield;

   function geti (L : in State; index : in Integer; i : in Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_geti(L.L,
                                        C.int(index),
                                        Long_Long_Integer(i)))
     );

   procedure geti (L : in State; index : in Integer; i : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_geti(L.L, C.int(index), Long_Long_Integer(i));
   end geti;

   function gettable (L : in State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_gettable(L.L, C.int(index)))
     );

   procedure gettable (L : in State; index : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_gettable(L.L, C.int(index));
   end gettable;

   function next (L : in State; index : in Integer) return Boolean is
     (Internal.lua_next(L.L, C.int(index)) /= 0);

   function rawget (L : in State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawget(L.L, C.int(index)))
     );

   procedure rawget (L : in State; index : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_rawget(L.L, C.int(index));
   end rawget;

   function rawgeti (L : in State; index : in Integer; i : in Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawgeti(L.L,
                                           C.int(index),
                                           Long_Long_Integer(i))
                     )
     );

   procedure rawgeti (L : in State; index : in Integer; i : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_rawgeti(L.L, C.int(index), Long_Long_Integer(i));
   end rawgeti;

   procedure rawset (L : in State; index : in Integer) is
   begin
      Internal.lua_rawset(L.L, C.int(index));
   end rawset;

   procedure rawseti (L : in State; index : in Integer; i : in Integer) is
   begin
      Internal.lua_rawseti(L.L, C.int(index), Long_Long_Integer(i));
   end rawseti;

   procedure setfield (L : in State; index : in Integer; k : in String) is
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Internal.lua_setfield(L.L, C.int(index), C_k);
      C.Strings.Free(C_k);
   end setfield;

   procedure seti (L : in State; index : in Integer; i : in Integer) is
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

   function getglobal (L : in State; name : in String) return Lua_Type is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result : C.int;
   begin
      Result := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_Name);
      return Int_To_Lua_Type(Result);
   end getglobal;

   procedure getglobal (L : in State; name : in String) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result : C.int;
   begin
      Result := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_Name);
   end getglobal;

   function getmetatable (L : in State; index : in Integer) return Boolean is
     (Internal.lua_getmetatable(L.L, C.int(index)) /= 0);

   procedure getmetatable (L : in State; index : in Integer) is
   begin
      if Internal.lua_getmetatable(L.L, C.int(index)) = 0 then
         raise Lua_Error with "No metatable exists for stack index: " &
           Integer'Image(index);
      end if;
   end getmetatable;

   procedure pushglobaltable (L : in State) is
      Discard : C.int;
   begin
      Discard := Internal.lua_rawgeti(L.L,
                                      C.int(RegistryIndex),
                                      Long_Long_Integer(RIDX_Globals));
   end pushglobaltable;

   procedure setglobal (L : in State; name : in String) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
   begin
      Internal.lua_setglobal(L.L, C_name);
      C.Strings.Free(C_name);
   end setglobal;

   procedure setmetatable (L : in State; index : in Integer) is
      Discard : C.int;
   begin
      -- According to the Lua documentation, lua_setmetatable does not have a
      -- return value, but according to lua.h it does...
      Discard := Internal.lua_setmetatable(L.L, C.int(index));
   end setmetatable;

   --
   -- *** Threads
   --

   function newthread (L : in State'Class) return Thread is
   begin
      return T : Thread do
         T.L := Internal.lua_newthread(L.L);
      end return;
   end newthread;

   function resume(L : in State'Class; from : in State'Class; nargs : Integer)
                   return Thread_Status is
      (Int_To_Thread_Status(Internal.lua_resume(L.L, from.l, C.int(nargs))));

   procedure xmove (from, to : in Thread; n : in Integer) is
   begin
      Internal.lua_xmove(from.L, to.L, C.int(n));
   end xmove;

   procedure yield (L : in State; nresults : Integer) is
      Discard : C.int;
   begin
      Discard := Internal.lua_yieldk(L.L, C.int(nresults), 0, null);
   end yield;

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

   function CFunction_Trampoline (L : System.Address) return C.int is
      S : Existing_State;
      f_index : C.int := C.Int(RegistryIndex-1);
      f_address : System.Address := Internal.lua_touserdata(L, f_index);
      f : AdaFunction := Address_To_AdaFunction(f_address);
   begin
      S.L := L;
      return C.int(f(S));
   end CFunction_Trampoline;

   --
   -- *** References
   --

   procedure Free is
     new Ada.Unchecked_Deallocation ( Object => Lua_Reference_Value,
                                      Name => Lua_Reference_Value_Access);

   function Ref (L : in State'Class; t : in Integer := RegistryIndex)
                 return Lua_Reference is
   begin
      return R : Lua_Reference do
         R.E := new Lua_Reference_Value;
         R.E.State := L.L;
         R.E.Table := C.int(t);
         R.E.Ref := AuxInternal.lual_ref(L.L, C.int(t));
         R.E.Count := 1;
      end return;
   end Ref;

   function Get (L : in State; R : Lua_Reference'Class) return Lua_Type is
   begin
      if R.E = null then
         raise Program_Error with "Empty Lua reference used";
      elsif R.E.State /= L.L then
         raise Program_Error with "Lua reference used on the wrong state!";
      end if;
      return Int_To_Lua_Type(Internal.lua_rawgeti(R.E.all.State,
                             R.E.all.Table,
                             Long_Long_Integer(R.E.all.Ref)));
   end Get;


   procedure Get (L : in State; R : Lua_Reference'Class) is
      Discard : C.int;
   begin
      if R.E = null then
         raise Program_Error with "Empty Lua reference used";
      elsif R.E.State /= L.L then
         raise Program_Error with "Lua reference used on the wrong state!";
      end if;
      Discard := Internal.lua_rawgeti(R.E.all.State,
                                      R.E.all.Table,
                                      Long_Long_Integer(R.E.all.Ref));
   end Get;

   overriding procedure Adjust (Object : in out Lua_Reference) is
   begin
      if Object.E /= null then
         Object.E.Count := Object.E.Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Lua_Reference) is
   begin
      if Object.E /= null then
         Object.E.Count := Object.E.Count - 1;
         if Object.E.Count = 0 then
            -- Note this relies on the Lua state not having been destroyed
            -- before the references stop being used.
            AuxInternal.lual_unref(Object.E.State,
                                   Object.E.Table,
                                   Object.E.Ref);
            Free(Object.E);
         end if;
      end if;
   end Finalize;


end Lua;
