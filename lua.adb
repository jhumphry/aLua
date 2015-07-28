-- Lua
-- an Ada 2012 interface to Lua

-- Copyright (c) 2015, James Humphry - see LICENSE.md for terms

with Ada.Unchecked_Conversion, Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int, Interfaces.C.size_t;
with Interfaces.C.Strings;
use type Interfaces.C.Strings.chars_ptr;

with System;
use type System.Address;

with Lua.Internal, Lua.AuxInternal, Lua.LibInternal;
with Ada.Finalization;

package body Lua is

   --
   -- *** Types only used internally
   --


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

   function Version (L : Lua_State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

   function Status (L : Lua_State) return Thread_Status is
      (Int_To_Thread_Status(Internal.lua_status(L.L)));

   function LoadString (L : in Lua_State;
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

   function LoadFile (L : in Lua_State;
                      Name : in String;
                      Mode : in Lua_ChunkMode := Binary_and_Text)
                      return Thread_Status is
      C_Name : C.Strings.chars_ptr := C.Strings.New_String(Name);
      C_Mode : C.Strings.chars_ptr
        := C.Strings.New_String(case Mode is
                                   when Binary => "b",
                                   when Text => "t",
                                   when Binary_and_Text => "bt"
                               );
      Result : C.int;
   begin
      Result := AuxInternal.luaL_loadfilex(L.L, C_Name, C_Mode);
      return Int_To_Thread_Status(Result);
   end Loadfile;

   --
   -- *** Calling, yielding and functions
   --

   procedure Call (L : in Lua_State;
                   nargs : in Integer;
                   nresults : in Integer) is
   begin
      Internal.lua_callk(L.L,
                         C.int(nargs),
                         C.int(nresults),
                         0,
                         null);
   end Call;

   function PCall (L : in Lua_State;
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

   procedure Register(L : in Lua_State; name : in String; f : in AdaFunction) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
   begin
      PushAdaClosure(L, f, 0);
      Internal.lua_setglobal(L.L, C_name);
      C.Strings.Free(C_name);
   end Register;

   --
   -- *** Pushing values to the stack
   --

   procedure PushAdaClosure (L : in Lua_State;
                             f : in AdaFunction;
                             n : in Natural) is
   begin
      Internal.lua_pushlightuserdata(L.L, AdaFunction_To_Address(f));
      L.Rotate(L.GetTop - n, 1);
      Internal.lua_pushcclosure(L.L, CFunction_Trampoline'Access, C.int(1 + n));
   end PushAdaClosure;

   procedure PushAdaFunction (L : in Lua_State; f : in AdaFunction) is
   begin
      PushAdaClosure(L, f, 0);
   end PushAdaFunction;

   procedure PushBoolean (L : in  Lua_State; b : in Boolean) is
   begin
      Internal.lua_pushboolean(L.L, C.int( (if b then 1 else 0) ) );
   end PushBoolean;

   procedure PushInteger (L : in Lua_State; n : in Lua_Integer) is
   begin
      Internal.lua_pushinteger(L.L, Internal.lua_Integer(n));
   end PushInteger;

   procedure PushNil (L : in Lua_State) is
   begin
      Internal.lua_pushnil(L.L);
   end PushNil;

   procedure PushNumber (L : in  Lua_State; n : in Lua_Number) is
   begin
      Internal.lua_pushnumber(L.L, Internal.lua_Number(n));
   end PushNumber;

   procedure PushString (L : in Lua_State; s : in String) is
      C_s : C.Strings.chars_ptr;
      Discard : C.Strings.chars_ptr;
   begin
      C_s := C.Strings.New_String(s);
      Discard := Internal.lua_pushstring(L.L, C_s);
      C.Strings.Free(C_s);
   end PushString;

   function PushThread (L : in Lua_State) return Boolean is
     (Internal.lua_pushthread(L.L) = 1);

   procedure PushThread (L : in Lua_State) is
      Discard : C.int;
   begin
      Discard := Internal.lua_pushthread(L.L);
   end PushThread;

   function StringToNumber (L : in Lua_State; s : in String) return Boolean is
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

   function ToAdaFunction (L : in Lua_State; index : in Integer)
                           return AdaFunction is
      Upvalue : System.Address;
      Discard : C.Strings.chars_ptr;
   begin
      Discard := Internal.lua_getupvalue(L.L, C.int(index), 1);
      if Discard = C.Strings.Null_Ptr then
         raise Constraint_Error
           with "Function referenced is not an AdaFunction";
      end if;
      Upvalue := Internal.lua_touserdata(L.L, -1);
      Internal.lua_settop(L.L, -2);
      return Address_To_AdaFunction(Upvalue);
   end ToAdaFunction;

   function ToBoolean (L : in Lua_State; index : in Integer) return Boolean is
      (Internal.lua_toboolean(L.L, C.int(Index)) /= 0);

   function ToInteger (L : in Lua_State; index : in Integer) return Lua_Integer is
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

   function ToNumber (L : in Lua_State; index : in Integer) return Lua_Number is
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

   function ToString (L : in Lua_State; index : in Integer) return String is
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

   function ToThread (L : in Lua_State; index : in Integer) return Lua_Thread is
   begin
      return R : Lua_Thread do
         R.L := Internal.lua_tothread(L.L, C.int(index));
      end return;
   end ToThread;

   --
   -- *** Operations on values
   --

   procedure Arith (L : in Lua_State; op : in Arith_Op) is
   begin
      Internal.lua_arith(L.L, Arith_Op_To_Int(op));
   end Arith;

   function Compare (L : in Lua_State;
                     index1 : in Integer;
                     index2 : in Integer;
                     op : in Comparison_Op) return Boolean is
     (Internal.lua_compare(L.L,
                           C.int(index1),
                           C.int(index2),
                           Comparison_Op_To_Int(op)) = 1);

   procedure Len (L : in  Lua_State; index : Integer) is
   begin
      Internal.lua_len(L.L, C.int(index));
   end Len;

   function RawEqual(L : in Lua_State; index1, index2 : in Integer)
                     return Boolean is
      (Internal.lua_rawequal(L.L, C.int(index1), C.int(index2)) /= 0);

   function RawLen (L : in Lua_State; index : Integer) return Integer is
      (Integer(Internal.lua_rawlen(L.L, C.int(index))));

   --
   -- *** Garbage Collector control
   ---

   procedure GC (L : in Lua_State; what : in GC_Op) is
      Discard : C.int;
   begin
      Discard := Internal.lua_gc(L.L, GC_Inputs_To_Int(what), 0);
   end GC;

   function GC (L : in Lua_State; what : in GC_Param; data : in Integer)
                return Integer is
      (Integer(Internal.lua_gc(L.L, GC_Inputs_To_Int(what), C.int(data))));

   function GC (L : in Lua_State) return Boolean is
       (Internal.lua_gc(L.L, GC_Inputs_To_Int(GCISRUNNING), 0) /= 0);

   --
   -- *** Stack manipulation and information
   --

   function AbsIndex (L : in Lua_State; idx : in Integer) return Integer is
     (Integer(Internal.lua_absindex(L.L, C.int(idx))));

   function CheckStack (L : in Lua_State; n : in Integer) return Boolean is
     (Internal.lua_checkstack(L.L, C.int(n)) /= 0);

   procedure Copy (L : in Lua_State; fromidx : in Integer; toidx : in Integer) is
   begin
      Internal.lua_copy(L.L, C.int(fromidx), C.int(toidx));
   end Copy;

   function GetTop (L : in Lua_State) return Integer is
     (Integer(Internal.lua_gettop(L.L)));

   procedure Insert (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), 1);
   end Insert;

   procedure Pop (L : in Lua_State; n : in Integer) is
   begin
      Internal.lua_settop(L.L, -C.int(n)-1);
   end Pop;

   procedure PushValue (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_pushvalue(L.L, C.int(index));
   end PushValue;

   procedure Remove (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(index), -1);
      Internal.lua_settop(L.L, -2);
   end Remove;

   procedure Replace (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_copy(L.L, -1, C.int(index));
      Internal.lua_settop(L.L, -2);
   end Replace;

   procedure Rotate (L : in Lua_State; idx : in Integer; n : in Integer) is
   begin
      Internal.lua_rotate(L.L, C.int(idx), C.int(n));
   end Rotate;

   procedure SetTop (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_settop(L.L, C.int(index));
   end SetTop;

   --
   -- *** Type information
   --

   function IsAdaFunction (L : in Lua_State; index : in Integer) return Boolean is
   begin
      if IsCFunction(L, index) then
         if Internal.lua_getupvalue(L.L, C.int(index),1) /= C.Strings.Null_Ptr then
            Internal.lua_settop(L.L, -2);
            return true;
         else
            Internal.lua_settop(L.L, -2);
            return false;
         end if;
      else
         return false;
      end if;
   end IsAdaFunction;

   function IsBoolean (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TBOOLEAN);

   function IsCFunction (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_iscfunction(L.L, C.int(index)) /= 0);

   function IsFunction (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TFUNCTION);

   function IsInteger (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isinteger(L.L, C.int(index)) /= 0);

   function IsLightuserdata (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TLIGHTUSERDATA);

   function IsNil (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TNIL);

   function IsNone (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TNONE);

   function IsNoneOrNil (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TNONE or Typeinfo(L, index) = TNIL);

   function IsNumber (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isnumber(L.L, C.int(index)) /= 0);

   function IsString (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isstring(L.L, C.int(index)) /= 0);

   function IsTable (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TTABLE);

   function IsThread (L : in Lua_State; index : in Integer) return Boolean is
     (Typeinfo(L, index) = TTHREAD);

   function IsUserdata (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isuserdata(L.L, C.int(index)) /= 0);

   function TypeInfo (L : in Lua_State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_type(L.L, C.int(index)))
     );

   function TypeName (L : in Lua_State; tp : in Lua_Type) return String is
     (
      C.Strings.Value(Internal.lua_typename(L.L, C.int(Lua_Type_To_Int(tp))))
     );

   --
   -- *** Table Manipulation
   --

   procedure CreateTable (L : in Lua_State;
                          narr : in Integer := 0;
                          nrec : in Integer := 0) is
   begin
      Internal.lua_createtable(L.L, C.int(narr), C.int(nrec));
   end CreateTable;

   procedure NewTable (L : in Lua_State) is
   begin
      Internal.lua_createtable(L.L, 0, 0);
   end NewTable;

   function GetField (L : in Lua_State; index : in Integer; k : in String)
                      return Lua_Type is
      Result : C.int;
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Result := Internal.lua_getfield(L.L,
                                       C.int(index),
                                       C_k);
      C.Strings.Free(C_k);
      return Int_To_Lua_Type(Result);
   end GetField;

   procedure GetField (L : in Lua_State; index : in Integer; k : in String) is
      Discard : C.int;
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Discard := Internal.lua_getfield(L.L,
                                       C.int(index),
                                       C_k);
      C.Strings.Free(C_k);
   end GetField;

   function Geti (L : in Lua_State; index : in Integer; i : in Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_geti(L.L,
                                        C.int(index),
                                        Long_Long_Integer(i)))
     );

   procedure Geti (L : in Lua_State; index : in Integer; i : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_geti(L.L, C.int(index), Long_Long_Integer(i));
   end Geti;

   function GetTable (L : in Lua_State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_gettable(L.L, C.int(index)))
     );

   procedure GetTable (L : in Lua_State; index : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_gettable(L.L, C.int(index));
   end GetTable;

   function Next (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_next(L.L, C.int(index)) /= 0);

   function RawGet (L : in Lua_State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawget(L.L, C.int(index)))
     );

   procedure RawGet (L : in Lua_State; index : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_rawget(L.L, C.int(index));
   end RawGet;

   function RawGeti (L : in Lua_State; index : in Integer; i : in Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawgeti(L.L,
                                           C.int(index),
                                           Long_Long_Integer(i))
                     )
     );

   procedure RawGeti (L : in Lua_State; index : in Integer; i : in Integer) is
     Discard : C.int;
   begin
      Discard := Internal.lua_rawgeti(L.L, C.int(index), Long_Long_Integer(i));
   end RawGeti;

   procedure RawSet (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_rawset(L.L, C.int(index));
   end RawSet;

   procedure RawSeti (L : in Lua_State; index : in Integer; i : in Integer) is
   begin
      Internal.lua_rawseti(L.L, C.int(index), Long_Long_Integer(i));
   end RawSeti;

   procedure SetField (L : in Lua_State; index : in Integer; k : in String) is
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Internal.lua_setfield(L.L, C.int(index), C_k);
      C.Strings.Free(C_k);
   end SetField;

   procedure Seti (L : in Lua_State; index : in Integer; i : in Integer) is
   begin
      Internal.lua_seti(L.L, C.int(index), Long_Long_Integer(i));
   end Seti;

   procedure SetTable (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_settable(L.L, C.int(index));
   end settable;

   --
   -- *** Globals and Metatables
   --

   function GetGlobal (L : in Lua_State; name : in String) return Lua_Type is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result : C.int;
   begin
      Result := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_Name);
      return Int_To_Lua_Type(Result);
   end GetGlobal;

   procedure GetGlobal (L : in Lua_State; name : in String) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result : C.int;
   begin
      Result := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_Name);
   end GetGlobal;

   function GetMetatable (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_getmetatable(L.L, C.int(index)) /= 0);

   procedure GetMetatable (L : in Lua_State; index : in Integer) is
   begin
      if Internal.lua_getmetatable(L.L, C.int(index)) = 0 then
         raise Lua_Error with "No metatable exists for stack index: " &
           Integer'Image(index);
      end if;
   end GetMetatable;

   procedure PushGlobalTable (L : in Lua_State) is
      Discard : C.int;
   begin
      Discard := Internal.lua_rawgeti(L.L,
                                      C.int(RegistryIndex),
                                      Long_Long_Integer(RIDX_Globals));
   end PushGlobalTable;

   procedure SetGlobal (L : in Lua_State; name : in String) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
   begin
      Internal.lua_setglobal(L.L, C_name);
      C.Strings.Free(C_name);
   end SetGlobal;

   procedure SetMetatable (L : in Lua_State; index : in Integer) is
      Discard : C.int;
   begin
      -- According to the Lua documentation, lua_setmetatable does not have a
      -- return value, but according to lua.h it does...
      Discard := Internal.lua_setmetatable(L.L, C.int(index));
   end SetMetatable;

   --
   -- *** Threads
   --

   function IsYieldable (L : in Lua_State'Class) return Boolean is
   begin
      return Internal.lua_isyieldable(L.L) /= 0;
   end IsYieldable;

   function NewThread (L : in Lua_State'Class) return Lua_Thread is
   begin
      return T : Lua_Thread do
         T.L := Internal.lua_newthread(L.L);
      end return;
   end NewThread;

   function Resume(L : in Lua_State'Class; from : in Lua_State'Class; nargs : Integer)
                   return Thread_Status is
      (Int_To_Thread_Status(Internal.lua_resume(L.L, from.l, C.int(nargs))));

   procedure XMove (from, to : in Lua_Thread; n : in Integer) is
   begin
      Internal.lua_xmove(from.L, to.L, C.int(n));
   end XMove;

   procedure Yield (L : in Lua_State; nresults : Integer) is
      Discard : C.int;
   begin
      Discard := Internal.lua_yieldk(L.L, C.int(nresults), 0, null);
   end Yield;

   --
   -- *** Resource Management ***
   --

   procedure Initialize (Object : in out Lua_State) is
   begin
      Object.L := AuxInternal.luaL_newstate;
   end Initialize;

   procedure Finalize (Object : in out Lua_State) is
   begin
      Internal.lua_close(Object.L);
   end Finalize;

   --
   -- *** Trampolines
   --

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

   function Ref (L : in Lua_State'Class; t : in Integer := RegistryIndex)
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

   function Get (L : in Lua_State; R : Lua_Reference'Class) return Lua_Type is
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


   procedure Get (L : in Lua_State; R : Lua_Reference'Class) is
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
