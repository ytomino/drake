with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.string;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);
   use type C.char_ptr;
   use type C.char_ptr_ptr;
   use type C.signed_int;
   use type C.ptrdiff_t;

   package char_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.char,
         C.char_ptr);
   package char_ptr_ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         C.char_ptr,
         C.char_ptr_ptr);

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
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return C.stdlib.getenv (C_Name (0)'Access);
   end getenv;

   --  implementation

   function Value (Name : String) return String is
      Result : constant C.char_ptr := getenv (Name);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (
            char_ptr_Conv.To_Address (Result));
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
   begin
      return getenv (Name) /= null;
   end Exists;

   procedure Set (Name : String; Value : String) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Z_Value : String := Value & Character'Val (0);
      C_Value : C.char_array (C.size_t);
      for C_Value'Address use Z_Value'Address;
   begin
      if C.stdlib.setenv (C_Name (0)'Access, C_Value (0)'Access, 1) /= 0 then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      if C.stdlib.unsetenv (C_Name (0)'Access) /= 0 then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant C.char_ptr_ptr := System.Environment_Block;
      I : C.char_ptr_ptr := Block;
   begin
      while I.all /= null loop
         I := I + 1;
      end loop;
      while I /= Block loop
         I := I + (-1);
         Clear (Name (Cursor (char_ptr_ptr_Conv.To_Address (I))).Element.all);
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      I : C.char_ptr_ptr := System.Environment_Block;
   begin
      while I.all /= null loop
         Process (
            Name (Cursor (char_ptr_ptr_Conv.To_Address (I))).Element.all,
            Value (Cursor (char_ptr_ptr_Conv.To_Address (I))).Element.all);
         I := I + 1;
      end loop;
   end Iterate;

   function Iterate return Iterator is
   begin
      return (null record);
   end Iterate;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all
         /= null;
   end Has_Element;

   function Name (Position : Cursor)
      return References.String.Slicing.Constant_Reference_Type
   is
      subtype Fixed_String is String (Positive);
      S : Fixed_String;
      for S'Address use char_ptr_Conv.To_Address (
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all);
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
      for S'Address use char_ptr_Conv.To_Address (
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all);
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

   overriding function First (Object : Iterator) return Cursor is
      pragma Unreferenced (Object);
   begin
      return Cursor (char_ptr_ptr_Conv.To_Address (System.Environment_Block));
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return Cursor (char_ptr_ptr_Conv.To_Address (
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)) + 1));
   end Next;

end Ada.Environment_Variables;
