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
         Clear (Constant_Reference (Cursor (I)).Name.all);
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : C.char_ptr_ptr := Inside.Environment_Block;
   begin
      while I.all /= null loop
         declare
            pragma Warnings (Off, "variable ""Ref"" is not referenced");
            Ref : Constant_Reference_Type := Constant_Reference (Cursor (I));
            pragma Warnings (On, "variable ""Ref"" is not referenced");
         begin
            Process (Ref.Name.all, Ref.Value.all);
         end;
         I := I + 1;
      end loop;
   end Iterate;

   function Iterate return Iterator is
   begin
      return (null record);
   end Iterate;

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

   function Constant_Reference (Position : Cursor)
      return Constant_Reference_Type
   is
      p : constant C.char_ptr := Position.all;
      Length : constant Natural := Integer (C.string.strlen (p));
      S : String (1 .. Length);
      for S'Address use p.all'Address;
      Name_Last : Natural := Length;
      Value_First : Positive := Length + 1;
   begin
      for I in S'Range loop
         if S (I) = '=' then
            Name_Last := I - 1;
            Value_First := I + 1;
            exit;
         end if;
      end loop;
      --  see s-arrays.adb
      return Result : aliased Constant_Reference_Type := (
         Name => S'Unrestricted_Access, -- dummy
         Value => S'Unrestricted_Access,
         Name_First => 1,
         Name_Last => Name_Last,
         Value_First => Value_First,
         Value_Last => Length)
      do
         declare
            type Repr is record
               Data : System.Address;
               Constraints : System.Address;
            end record;
            pragma Suppress_Initialization (Repr);
            Name_R : Repr;
            for Name_R'Address use Result.Name'Address;
            Value_R : Repr;
            for Value_R'Address use Result.Value'Address;
         begin
            Name_R.Data := S (Result.Name_First)'Address;
            Name_R.Constraints := Result.Name_First'Address;
            Value_R.Data := S (Result.Value_First)'Address;
            Value_R.Constraints := Result.Value_First'Address;
         end;
      end return;
   end Constant_Reference;

end Ada.Environment_Variables;
