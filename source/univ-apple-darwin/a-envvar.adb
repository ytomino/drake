with Ada.Environment_Variables.Inside;
with Ada.Unchecked_Conversion;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.string;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);
   use type C.char_ptr;
   use type C.char_ptr_ptr;
   use type C.signed_int;
   use type C.ptrdiff_t;

   function "+" (Left : C.char_ptr_ptr; Right : C.ptrdiff_t)
      return C.char_ptr_ptr;
   function "+" (Left : C.char_ptr_ptr; Right : C.ptrdiff_t)
      return C.char_ptr_ptr
   is
      function I is new Unchecked_Conversion (C.char_ptr_ptr, C.ptrdiff_t);
      function P is new Unchecked_Conversion (C.ptrdiff_t, C.char_ptr_ptr);
   begin
      return P (I (Left) + Right * (C.char_ptr'Size / Standard'Storage_Unit));
   end "+";

   function getenv (Name : String) return C.char_ptr;
   function getenv (Name : String) return C.char_ptr is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      return C.stdlib.getenv (C_Name (0)'Access);
   end getenv;

   --  implementation

   function Value (Name : String) return String is
      Result : constant C.char_ptr := getenv (Name);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (Result.all'Address);
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
   begin
      return getenv (Name) /= null;
   end Exists;

   procedure Set (Name : String; Value : String) is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Value_Length : constant C.size_t := Value'Length;
      C_Value : C.char_array (0 .. Value_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      Dummy := C.string.memcpy (
         C.void_ptr (C_Value'Address),
         C.void_const_ptr (Value'Address),
         Value_Length);
      C_Value (Value_Length) := C.char'Val (0);
      if C.stdlib.setenv (C_Name (0)'Access, C_Value (0)'Access, 1) /= 0 then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      if C.stdlib.unsetenv (C_Name (0)'Access) /= 0 then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant C.char_ptr_ptr := Inside.Environment_Block;
      I : C.char_ptr_ptr := Block;
   begin
      while I.all /= null loop
         I := I + 1;
      end loop;
      while I /= Block loop
         I := I + (-1);
         Clear (Name (Cursor (I)).Element.all);
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : C.char_ptr_ptr := Inside.Environment_Block;
   begin
      while I.all /= null loop
         Process (
            Name (Cursor (I)).Element.all,
            Value (Cursor (I)).Element.all);
         I := I + 1;
      end loop;
   end Iterate;

   function Iterate return Iterator is
   begin
      return (null record);
   end Iterate;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Has_Element;

   function Name (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type
   is
      subtype Fixed_String is String (Positive);
      S : Fixed_String;
      for S'Address use Position.all.all'Address;
      I : Positive := 1;
   begin
      while S (I) /= '=' and then S (I) /= Character'Val (0) loop
         I := I + 1;
      end loop;
      return References.String.Slicing.Constant_Slice (
         S'Unrestricted_Access,
         1,
         I - 1);
   end Name;

   function Value (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type
   is
      subtype Fixed_String is String (Positive);
      S : Fixed_String;
      for S'Address use Position.all.all'Address;
      First : Positive;
      Last : Natural;
      I : Positive := 1;
   begin
      loop
         if S (I) = '=' then
            First := I + 1;
            Last := I + Natural (
               C.string.strlen ((C.char (S (I + 1))'Unrestricted_Access)));
            exit;
         elsif S (I) = Character'Val (0) then
            First := I;
            Last := I - 1;
            exit;
         end if;
         I := I + 1;
      end loop;
      return References.String.Slicing.Constant_Slice (
         S'Unrestricted_Access,
         First,
         Last);
   end Value;

   function First (Object : Iterator) return Cursor is
      pragma Unreferenced (Object);
      Result : Cursor := Cursor (Inside.Environment_Block);
   begin
      if Result.all = null then
         Result := null;
      end if;
      return Result;
   end First;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
      pragma Unreferenced (Object);
      Result : Cursor := Cursor (C.char_ptr_ptr (Position) + 1);
   begin
      if Result.all = null then
         Result := null;
      end if;
      return Result;
   end Next;

end Ada.Environment_Variables;
