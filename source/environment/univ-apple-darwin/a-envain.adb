with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Environment_Block;
with C.stdlib;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);
   use type C.char_ptr;
   use type C.char_ptr_ptr;
   use type C.signed_int;
   use type C.ptrdiff_t;

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

   function Reference (Name : String) return Character_Access is
      function C is new Unchecked_Conversion (C.char_ptr, Character_Access);
   begin
      return C (getenv (Name));
   end Reference;

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

   function Reference (Position : Cursor) return Character_Access is
      function C is new Unchecked_Conversion (C.char_ptr, Character_Access);
   begin
      return C (char_ptr_ptr_Conv.To_Pointer (System.Address (Position)).all);
   end Reference;

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
