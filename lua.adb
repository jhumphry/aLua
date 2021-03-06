-- Lua
-- an Ada 2012 interface to Lua

-- Copyright (c) 2015-2021, James Humphry - see LICENSE for terms

with Ada.Unchecked_Conversion, Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with Ada.Streams;

with Interfaces; use Interfaces;
with Interfaces.C;
use type Interfaces.C.int, Interfaces.C.size_t, Interfaces.C.ptrdiff_t;
with Interfaces.C.Strings;
use type Interfaces.C.Strings.chars_ptr;
with Interfaces.C.Pointers;

with System;
use type System.Address;

with Lua.Internal, Lua.AuxInternal;

package body Lua is

   --
   -- *** Routines only used internally
   --

   function Stream_Lua_Writer (L : void_ptr;
                               p : void_ptr;
                               sz : C.size_t;
                               ud : void_ptr)
                               return C.int with Convention => C;

   function String_Lua_Reader (L : void_ptr;
                               data : void_ptr;
                               size : access C.size_t)
                               return C.Strings.chars_ptr
     with Convention => C;

   function Stream_Lua_Reader (L : void_ptr;
                               data : void_ptr;
                               size : access C.size_t)
                               return C.Strings.chars_ptr
     with Convention => C;

   function CFunction_Trampoline (L : System.Address) return Interfaces.C.int
     with Convention => C;

   --
   -- *** Types only used internally
   --

   -- Used by String_Lua_Reader to read strings
   type String_Details is
      record
         S_Address : System.Address;
         S_Length : Natural;
         Readable : Boolean := False;
      end record;

   type Stream_Details(Buffer_Size : Ada.Streams.Stream_Element_Count) is
      record
         S : access Ada.Streams.Root_Stream_Type'Class;
         Buffer : aliased Ada.Streams.Stream_Element_Array(1..Buffer_Size)
           with Convention => C;
      end record;

   type Stream_Element_Access is not null access all Ada.Streams.Stream_Element;

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

   function GC_Op_To_Int is new
     Ada.Unchecked_Conversion(Source => GC_Op,
                              Target => C.int);

   function GC_Param_To_Int is new
     Ada.Unchecked_Conversion(Source => GC_Param,
                              Target => C.int);

   function Lua_Type_To_Int is new Ada.Unchecked_Conversion(Source => Lua_Type,
                                                            Target => C.int);

   function Int_To_Lua_Type is new Ada.Unchecked_Conversion(Source => C.int,
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

   package Stream_Access_Conversions is new
     System.Address_To_Access_Conversions(Ada.Streams.Root_Stream_Type'Class);

   package String_Details_Access_Conversions is new
     System.Address_To_Access_Conversions(String_Details);

   package Char_Access_Conversions is new
     System.Address_To_Access_Conversions(C.char);

   function Char_Object_Pointer_To_Chars_Ptr is new
     Ada.Unchecked_Conversion(Source => Char_Access_Conversions.Object_Pointer,
                              Target => C.Strings.chars_ptr);

   package Stream_Details_Access_Conversions is new
     System.Address_To_Access_Conversions(Stream_Details);

   function Stream_Element_Access_To_Chars_Ptr is new
     Ada.Unchecked_Conversion(Source => Stream_Element_Access,
                              Target => C.Strings.chars_ptr);

   --
   -- *** Conversions between a C void * and a Stream_Element_Array
   --

   package Void_Ptr_To_Stream_Array is new
      Interfaces.C.Pointers(Index => Ada.Streams.Stream_Element_Offset,
                            Element => Ada.Streams.Stream_Element,
                            Element_Array => Ada.Streams.Stream_Element_Array,
                            Default_Terminator => 0);

   package Stream_Element_Access_Conversions is new
     System.Address_To_Access_Conversions(Ada.Streams.Stream_Element);

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

   -- The Stream_Lua_Writer is an internal Lua_Writer where the userdata pointer
   -- is a Stream_Access type from Ada.Streams.Stream_IO. It should therefore
   -- support writing to and from any type of stream.

   function Stream_Lua_Writer (L : void_ptr;
                               p : void_ptr;
                               sz : C.size_t;
                               ud : void_ptr)
                               return C.int  is
      pragma Unreferenced (L);

      package SAC renames Stream_Access_Conversions;
      package SEAC renames Stream_Element_Access_Conversions;
      package VPTSA renames Void_Ptr_To_Stream_Array;
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      Output_Stream_Access : constant Stream_Access
        := Stream_Access(SAC.To_Pointer(ud));

      -- First we need to convert the address of the data to dump to an access
      -- value using System.Address_To_Access_Conversions, then we need to
      -- convert this to the equivalent access value in Interfaces.C.Pointers.
      -- Once we know the length of the array, we can then convert it back to a
      -- true Ada array.
      Output_Data_Access : constant VPTSA.Pointer
        := VPTSA.Pointer(SEAC.To_Pointer(p));

      -- This calculation is intended to deal with (most) cases in which
      -- Stream_Element is not a single byte. Why anyone would do that I
      -- don't know, but it seems possible according to the RM. Cases in which
      -- Stream_Element is a fractional number of bytes will not work, but
      -- I can't see how an Ada system using such a convention could ever
      -- be expected to interoperate with C code.
      Output_Data_Length : constant C.ptrdiff_t
        := (C.ptrdiff_t(sz) * 8) / Stream_Element'Size;

      Output_Data : constant Stream_Element_Array
        := VPTSA.Value(Ref => Output_Data_Access,
                       Length => Output_Data_Length);

   begin
      Output_Stream_Access.Write(Item => Output_Data);
      return 0;
   exception
      when Status_Error | Mode_Error | Device_Error  =>
         return 1;
         -- Other exceptions are deliberately not handled as they are more
         -- likely to indicate serious internal problems, for example being
         -- sent a null pointer as the data to write.
   end Stream_Lua_Writer;

   procedure DumpFile(L : in Lua_State;
                      Name : in String;
                      Strip : in Boolean := False) is
      use Ada.Streams.Stream_IO;

      Output_File : File_Type;
   begin
      Create(File => Output_File, Mode => Out_File, Name => Name);
      DumpStream(L => L,
                 Output_Stream => Stream(Output_File),
                 Strip => Strip);
      Close(Output_File);
   end DumpFile;

   procedure DumpStream(L : in Lua_State;
                        Output_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                        Strip : in Boolean := False)  is
      package SAC renames Stream_Access_Conversions;
      Result : C.int;
      Stream_Pointer : constant SAC.Object_Pointer
        := SAC.Object_Pointer(Output_Stream);
   begin
      Result := Internal.lua_dump(L.L,
                                  Stream_Lua_Writer'Access,
                                  SAC.To_Address(Stream_Pointer),
                                  (if Strip then 1 else 0));
      if Result /= 0 then
         raise Lua_Error with "Could not dump Lua chunk to stream";
      end if;
   end DumpStream;

   function String_Lua_Reader (L : void_ptr;
                               data : void_ptr;
                               size : access C.size_t)
                               return C.Strings.chars_ptr is
      pragma Unreferenced (L);
      SDA : constant access String_Details
        := String_Details_Access_Conversions.To_Pointer(data);

      S_Object_Ptr : constant Char_Access_Conversions.Object_Pointer
        := Char_Access_Conversions.To_Pointer(SDA.S_Address);

      S_chars_ptr : constant C.Strings.chars_ptr
        := Char_Object_Pointer_To_Chars_Ptr(S_Object_Ptr);
   begin
      if SDA.Readable then
         SDA.Readable := False;
         size.all := C.size_t(SDA.S_Length * String'Component_Size / 8);
         return S_chars_ptr;
      else
         size.all := 0;
         return C.Strings.Null_Ptr;
      end if;
   end String_Lua_Reader;

   function LoadString (L : in Lua_State;
                        S : aliased String;
                        ChunkName : in String := "";
                        Mode : Lua_ChunkMode := Binary_and_Text)
                        return Thread_Status is
      Result : C.int;
      C_ChunkName : C.Strings.chars_ptr := C.Strings.New_String(ChunkName);
      C_Mode : C.Strings.chars_ptr
        := C.Strings.New_String(case Mode is
                                   when Binary => "b",
                                   when Text => "t",
                                   when Binary_and_Text => "bt"
                               );
      To_Load : aliased String_Details := (S_Address => S'Address,
                                           S_Length => S'Length,
                                           Readable => True);
   begin
      if C.char_array'Component_Size /= String'Component_Size then
         raise Program_Error with
           "Ada and C strings are not sufficiently compatible for direct " &
           "loading - only LoadString_By_Copy can be used on this system.";
      end if;
      Result := Internal.lua_load(L.L,
                                  String_Lua_Reader'Access,
                                  To_Load'Address,
                                  C_ChunkName,
                                  C_Mode);
      C.Strings.Free(C_ChunkName);
      C.Strings.Free(C_Mode);
      return Int_To_Thread_Status(Result);
   end LoadString;

   function LoadString_By_Copy (L : in Lua_State;
                                S : in String)
                                return Thread_Status is
      CS : C.Strings.chars_ptr;
      Result : C.int;
   begin
      CS := C.Strings.New_String(S);
      Result := AuxInternal.luaL_loadstring(L.L, CS);
      C.Strings.Free(CS);
      return Int_To_Thread_Status(Result);
   end;

   function Stream_Lua_Reader (L : void_ptr;
                               data : void_ptr;
                               size : access C.size_t)
                               return C.Strings.chars_ptr is
      pragma Unreferenced (L);

      use Ada.Streams;

      Stream_Detail : access Stream_Details
        := Stream_Details_Access_Conversions.To_Pointer(data);

      Buffer_Ptr :constant C.Strings.chars_ptr
        := Stream_Element_Access_To_Chars_Ptr(Stream_Detail.Buffer(1)'Access);

      Last_Read : Stream_Element_Offset;
   begin
      Read(Stream => Stream_Detail.S.all,
           Item => Stream_Detail.Buffer,
           Last => Last_Read);
      if Last_Read /= 0 then
         size.all := C.size_t(Last_Read * Stream_Element'Size / 8);
         return Buffer_Ptr;
      else
         size.all := 0;
         return C.Strings.Null_Ptr;
      end if;
   end Stream_Lua_Reader;

   function LoadFile (L : in Lua_State;
                      Name : in String;
                      ChunkName : in String := "";
                      Mode : in Lua_ChunkMode := Binary_and_Text;
                      Buffer_Size : in Positive := 256)
                      return Thread_Status is
      use Ada.Streams.Stream_IO;

      Input_File : File_Type;
      Result : Thread_Status;

   begin
      Open(File => Input_File,
           Mode => In_File,
           Name => Name);
      Result := LoadStream(L => L,
                           Input_Stream => Stream(Input_File),
                           ChunkName => ChunkName,
                           Mode => Mode,
                           Buffer_Size => Buffer_Size);
      Close(Input_File);
      return Result;
   end LoadFile;

   function LoadStream (L : in Lua_State;
                        Input_Stream : in Ada.Streams.Stream_IO.Stream_Access;
                        ChunkName : in String := "";
                        Mode : in Lua_ChunkMode := Binary_and_Text;
                        Buffer_Size : in Positive := 256)
                        return Thread_Status is
      use Ada.Streams;

      C_ChunkName : C.Strings.chars_ptr := C.Strings.New_String(ChunkName);
      C_Mode : C.Strings.chars_ptr
        := C.Strings.New_String(case Mode is
                                   when Binary => "b",
                                   when Text => "t",
                                   when Binary_and_Text => "bt"
                               );
      Stream_Detail : aliased Stream_Details(Stream_Element_Count(Buffer_Size));
      Result : C.int;

   begin
      Stream_Detail.S := Input_Stream;
      Result := Internal.lua_load(L.L,
                                  Stream_Lua_Reader'Access,
                                  Stream_Detail'Address,
                                  C_ChunkName,
                                  C_Mode);
      C.Strings.Free(C_ChunkName);
      C.Strings.Free(C_Mode);
      return Int_To_Thread_Status(Result);
   end LoadStream;

   function Status (L : Lua_State) return Thread_Status is
      (Int_To_Thread_Status(Internal.lua_status(L.L)));

   function Version (L : Lua_State) return Long_Float is
      (Long_Float(Internal.lua_version(L.L).all));

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

   procedure Call_Function (L : in Lua_State;
                            name : in String;
                            nargs : in Integer;
                            nresults : in Integer) is
   begin
      GetGlobal(L, name);
      if not IsFunction(L, -1) then
         raise Lua_Error with "Attempting to call a value that is not a Lua function";
      end if;
      Rotate(L, -1-nargs, 1);
      Internal.lua_callk(L.L,
                         C.int(nargs),
                         C.int(nresults),
                         0,
                         null);
   end Call_Function;

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

   function PCall_Function (L : in Lua_State;
                            name : in String;
                            nargs : in Integer;
                            nresults : in Integer;
                            msgh : in Integer := 0)
                            return Thread_Status is
      Result : C.int;
   begin
      GetGlobal(L, name);
      if not IsFunction(L, -1) then
         raise Lua_Error with "Attempting to call a value that is not a Lua function";
      end if;
      Rotate(L, -1-nargs, 1);
      Result := Internal.lua_pcallk(L.L,
                                   C.int(nargs),
                                   C.int(nresults),
                                   C.int(msgh),
                                   0,
                                   null
                                  );
      return Int_To_Thread_Status(Result);
     end PCall_Function;

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
      -- Not interested if this thread is the main thread of its state, so the
      -- boolean result is discarded.
      Discard := Internal.lua_pushthread(L.L);
   end PushThread;

   procedure SetUserValue (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_setuservalue(L.L, C.int(index));
   end SetUserValue;

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
      Upvalue_Name : C.Strings.chars_ptr;
   begin
      Upvalue_Name := Internal.lua_getupvalue(L.L, C.int(index), 1);
      if Upvalue_Name = C.Strings.Null_Ptr then
         raise Lua_Error with "Function referenced is not an AdaFunction";
      end if;
      Upvalue := Internal.lua_touserdata(L.L, -1);
      Internal.lua_settop(L.L, -2);
      return Address_To_AdaFunction(Upvalue);
   end ToAdaFunction;

   function ToBoolean (L : in Lua_State; index : in Integer) return Boolean is
      (Internal.lua_toboolean(L.L, C.int(index)) /= 0);

   function ToInteger (L : in Lua_State; index : in Integer) return Lua_Integer is
      isnum : aliased C.int := 0;
      result : Internal.lua_Integer;
   begin
      result := Internal.lua_tointegerx(L.L , C.int(index), isnum'Access);
      if isnum = 0 then
         raise Lua_Error with "Value at Lua stack index "
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
         raise Lua_Error with "Value at Lua stack index "
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
         raise Lua_Error with "Value at Lua stack index "
           & Integer'Image(index)
           & " is not convertible to a string.";
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
         if R.L = System.Null_Address then
            raise Lua_Error with "Value at Lua stack index "
              & Integer'Image(index)
              & " is not a thread value.";
         end if;
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

   procedure Concat (L : in Lua_State; n : in Integer) is
   begin
      Internal.lua_concat(L.L, C.int(n));
   end Concat;

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
      -- For the operations within subtype GC_Op, this will not return anything
      -- interesting
      Discard := Internal.lua_gc(L.L, GC_Op_To_Int(what), 0);
   end GC;

   function GC (L : in Lua_State; what : in GC_Param; data : in Integer)
                return Integer is
      (Integer(Internal.lua_gc(L.L, GC_Param_To_Int(what), C.int(data))));

   function GC_IsRunning (L : in Lua_State) return Boolean is
       (Internal.lua_gc(L.L, GCISRUNNING, 0) /= 0);

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
      use Lua.Internal;
   begin
      return lua_tocfunction(L.L, C.int(index)) = CFunction_Trampoline'Access;
        -- As CFunction_Trampoline is not visible outside this body, and this
        -- package will always create closure correctly, it is not necessary
        -- to check that the required upvalue exists.
   end IsAdaFunction;

   function IsBoolean (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TBOOLEAN);

   function IsCFunction (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_iscfunction(L.L, C.int(index)) /= 0);

   function IsFunction (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TFUNCTION);

   function IsInteger (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isinteger(L.L, C.int(index)) /= 0);

   function IsLightuserdata (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TLIGHTUSERDATA);

   function IsNil (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TNIL);

   function IsNone (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TNONE);

   function IsNoneOrNil (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TNONE or TypeInfo(L, index) = TNIL);

   function IsNumber (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isnumber(L.L, C.int(index)) /= 0);

   function IsString (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_isstring(L.L, C.int(index)) /= 0);

   function IsTable (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TTABLE);

   function IsThread (L : in Lua_State; index : in Integer) return Boolean is
     (TypeInfo(L, index) = TTHREAD);

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

   function TypeName (L : in Lua_State; index : in Integer) return String is
     (TypeName(L, TypeInfo(L, index)));

   function Userdata_Name (L : in Lua_State; index : in Integer) return String is
      Has_Metatable : Boolean;
      Name_Field_Type : Lua_Type;
   begin
      Has_Metatable := GetMetatable(L, index);
      if not Has_Metatable then
         return "";
      end if;

      Name_Field_Type := L.GetField(-1, "__name");
      if Name_Field_Type = TNIL then
         L.Pop(1); -- just the metatable
         return "";
      elsif Name_Field_Type /= TSTRING then
         L.Pop(2); -- the metatable and the non-standard __name field
         return "";
      end if;

      declare
         Name : constant String := L.ToString(-1);
      begin
         if Name'Length > 4
           and then Name(Name'First..Name'First+3) = "Ada:"
         then
             L.Pop(2); -- the metatable and the __name field
            return (Name(Name'First+4..Name'Last));
         else
             L.Pop(2); -- the metatable and the non-Ada __name field
            return "";
         end if;
      end;

   end Userdata_Name;

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
      Result_Type : C.int;
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Result_Type := Internal.lua_getfield(L.L,
                                           C.int(index),
                                           C_k);
      C.Strings.Free(C_k);
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No value for key: '" & k & "'.";
      end if;
   end GetField;

   function Geti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_geti(L.L,
                                        C.int(index),
                                        Long_Long_Integer(i)))
     );

   procedure Geti (L : in Lua_State; index : in Integer; i : in Lua_Integer) is
      Result_Type : C.int;
   begin
      Result_Type := Internal.lua_geti(L.L, C.int(index), Long_Long_Integer(i));
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No value for key: '" & Lua_Integer'Image(i) &
           "'.";
      end if;
   end Geti;

   function GetTable (L : in Lua_State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_gettable(L.L, C.int(index)))
     );

   procedure GetTable (L : in Lua_State; index : in Integer) is
      Result_Type : C.int;
   begin
      Result_Type := Internal.lua_gettable(L.L, C.int(index));
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No value for key and target specified.";
      end if;
   end GetTable;

   function Next (L : in Lua_State; index : in Integer) return Boolean is
     (Internal.lua_next(L.L, C.int(index)) /= 0);

   function RawGet (L : in Lua_State; index : in Integer) return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawget(L.L, C.int(index)))
     );

   procedure RawGet (L : in Lua_State; index : in Integer) is
     Result_Type : C.int;
   begin
      Result_Type := Internal.lua_rawget(L.L, C.int(index));
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No value for key and target specified.";
      end if;
   end RawGet;

   function RawGeti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
                  return Lua_Type is
     (
      Int_To_Lua_Type(Internal.lua_rawgeti(L.L,
                                           C.int(index),
                                           Long_Long_Integer(i))
                     )
     );

   procedure RawGeti (L : in Lua_State; index : in Integer; i : in Lua_Integer) is
      Result_Type : C.int;
   begin
      Result_Type := Internal.lua_rawgeti(L.L, C.int(index), Long_Long_Integer(i));
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No value for key: '" & Lua_Integer'Image(i) &
           "'.";
      end if;
   end RawGeti;

   procedure RawSet (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_rawset(L.L, C.int(index));
   end RawSet;

   procedure RawSeti (L : in Lua_State; index : in Integer; i : in Lua_Integer)
   is
   begin
      Internal.lua_rawseti(L.L, C.int(index), Long_Long_Integer(i));
   end RawSeti;

   procedure SetField (L : in Lua_State; index : in Integer; k : in String) is
      C_k : C.Strings.chars_ptr := C.Strings.New_String(k);
   begin
      Internal.lua_setfield(L.L, C.int(index), C_k);
      C.Strings.Free(C_k);
   end SetField;

   procedure Seti (L : in Lua_State; index : in Integer; i : in Lua_Integer) is
   begin
      Internal.lua_seti(L.L, C.int(index), Long_Long_Integer(i));
   end Seti;

   procedure SetTable (L : in Lua_State; index : in Integer) is
   begin
      Internal.lua_settable(L.L, C.int(index));
   end SetTable;

   --
   -- *** Globals and Metatables
   --

   function GetGlobal (L : in Lua_State; name : in String) return Lua_Type is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result : C.int;
   begin
      Result := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_name);
      return Int_To_Lua_Type(Result);
   end GetGlobal;

   procedure GetGlobal (L : in Lua_State; name : in String) is
      C_name : C.Strings.chars_ptr := C.Strings.New_String(name);
      Result_Type : C.int;
   begin
      Result_Type := Internal.lua_getglobal(L.L, C_name);
      C.Strings.Free(C_name);
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "No global by the name of :'" & name & "' found.";
      end if;
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
      -- The global table should always exist so no test is performed.
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

   function Resume(L : in Lua_State'Class;
                   nargs : in Integer;
                   from : in Lua_State'Class := Null_Thread
                  )
                   return Thread_Status is
     (Int_To_Thread_Status(Internal.lua_resume(L.L, from.L, C.int(nargs))));

   procedure XMove (from, to : in Lua_Thread; n : in Integer) is
   begin
      Internal.lua_xmove(from.L, to.L, C.int(n));
   end XMove;

   procedure Yield (L : in Lua_State; nresults : Integer) is
      Discard : C.int;
   begin
      -- Return value does not appear to be useful.
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
      f_index : constant C.int := C.int(RegistryIndex-1);
      f_address : constant System.Address := Internal.lua_touserdata(L, f_index);
      f : constant AdaFunction := Address_To_AdaFunction(f_address);
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
         R.E.Ref := AuxInternal.luaL_ref(L.L, C.int(t));
         R.E.Count := 1;
      end return;
   end Ref;

   function Get (L : in Lua_State; R : Lua_Reference'Class) return Lua_Type is
   begin
      if R.E = null then
         raise Lua_Error with "Empty Lua reference used";
      elsif R.E.State /= L.L then
         raise Lua_Error with "Lua reference used on the wrong state!";
      end if;
      return Int_To_Lua_Type(Internal.lua_rawgeti(R.E.all.State,
                             R.E.all.Table,
                             Long_Long_Integer(R.E.all.Ref)));
   end Get;

   procedure Get (L : in Lua_State; R : Lua_Reference'Class) is
      Result_Type : C.int;
   begin
      if R.E = null then
         raise Lua_Error with "Empty Lua reference used";
      elsif R.E.State /= L.L then
         raise Lua_Error with "Lua reference used on the wrong state!";
      end if;
      Result_Type := Internal.lua_rawgeti(R.E.all.State,
                                          R.E.all.Table,
                                          Long_Long_Integer(R.E.all.Ref));
      if Int_To_Lua_Type(Result_Type) = TNIL then
         raise Lua_Error with "Lua reference somehow pointing to nil value.";
      end if;
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
            AuxInternal.luaL_unref(Object.E.State,
                                   Object.E.Table,
                                   Object.E.Ref);
            Free(Object.E);
         end if;
      end if;
   end Finalize;

end Lua;
