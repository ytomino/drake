with Ada.Unchecked_Conversion;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.string;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;
   use type C.char_ptr;
   use type C.char_ptr_ptr;
   use type C.signed_int;
   use type C.ptrdiff_t;

   package char_const_ptr_Conv is
      new System.Address_To_Constant_Access_Conversions (
         C.char,
         C.char_const_ptr);

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

   procedure Do_Separate (
      Item : not null access constant C.char;
      Name_Length : out C.size_t;
      Value : out C.char_const_ptr);
   procedure Do_Separate (
      Item : not null access constant C.char;
      Name_Length : out C.size_t;
      Value : out C.char_const_ptr)
   is
      P : C.char_ptr;
   begin
      P := C.string.strchr (Item, C.char'Pos ('='));
      if P /= null then
         Name_Length := C.size_t (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (P))
            - char_const_ptr_Conv.To_Address (C.char_const_ptr (Item)));
         Value := char_const_ptr_Conv.To_Pointer (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (P)) + 1);
      else
         Name_Length := C.string.strlen (Item);
         Value := char_const_ptr_Conv.To_Pointer (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (Item))
            + System.Storage_Elements.Storage_Offset (Name_Length));
      end if;
   end Do_Separate;

   --  implementation

   function Value (Name : String) return String is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Result : constant C.char_ptr := C.stdlib.getenv (C_Name (0)'Access);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (
            char_const_ptr_Conv.To_Address (C.char_const_ptr (Result)));
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      return C.stdlib.getenv (C_Name (0)'Access) /= null;
   end Exists;

   procedure Set (Name : String; Value : String; Error : out Boolean) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
      Z_Value : String := Value & Character'Val (0);
      C_Value : C.char_array (C.size_t);
      for C_Value'Address use Z_Value'Address;
   begin
      Error := C.stdlib.setenv (C_Name (0)'Access, C_Value (0)'Access, 1) < 0;
   end Set;

   procedure Clear (Name : String; Error : out Boolean) is
      Z_Name : String := Name & Character'Val (0);
      C_Name : C.char_array (C.size_t);
      for C_Name'Address use Z_Name'Address;
   begin
      Error := C.stdlib.unsetenv (C_Name (0)'Access) < 0;
   end Clear;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all
         /= null;
   end Has_Element;

   function Name (Position : Cursor) return String is
      Item : constant C.char_ptr :=
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all;
      subtype Fixed_String is String (Positive);
      Name : Fixed_String;
      for Name'Address use
         char_const_ptr_Conv.To_Address (C.char_const_ptr (Item));
      Name_Length : C.size_t;
      Value : C.char_const_ptr;
   begin
      Do_Separate (Item, Name_Length, Value);
      return Name (1 .. Natural (Name_Length));
   end Name;

   function Value (Position : Cursor) return String is
      Item : constant C.char_ptr :=
         char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all;
      Name_Length : C.size_t;
      Value : C.char_const_ptr;
   begin
      Do_Separate (Item, Name_Length, Value);
      return System.Zero_Terminated_Strings.Value (
         char_const_ptr_Conv.To_Address (Value));
   end Value;

   function First return Cursor is
   begin
      return Cursor (char_ptr_ptr_Conv.To_Address (System.Environment_Block));
   end First;

   function "+" (Left : Cursor; Right : Integer) return Cursor is
   begin
      return Cursor (char_ptr_ptr_Conv.To_Address (
         char_ptr_ptr_Conv.To_Pointer (System.Address (Left))
         + C.ptrdiff_t (Right)));
   end "+";

end Ada.Environment_Variables.Inside;
