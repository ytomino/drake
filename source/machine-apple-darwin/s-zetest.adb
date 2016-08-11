with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
package body System.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);
   use type C.size_t;

   function strlen (Item : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   procedure memcpy (s1, s2 : Address; n : C.size_t)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   --  implementation

   function Value (First : not null access constant C.char) return String is
   begin
      return Value (First, strlen (First));
   end Value;

   function Value (
      First : not null access constant C.char;
      Length : C.size_t)
      return String
   is
      type char_const_ptr is access constant C.char; -- local type
      for char_const_ptr'Storage_Size use 0;
      package Conv is
         new Address_To_Constant_Access_Conversions (C.char, char_const_ptr);
      Source : String (1 .. Natural (Length));
      for Source'Address use Conv.To_Address (char_const_ptr (First));
   begin
      return Source;
   end Value;

   procedure To_C (Source : String; Result : not null access C.char) is
      Dummy : C.size_t;
   begin
      To_C (Source, Result, Dummy);
   end To_C;

   procedure To_C (
      Source : String;
      Result : not null access C.char;
      Result_Length : out C.size_t)
   is
      --  Source and Result should not be overlapped
      type char_ptr is access all C.char; -- local type
      for char_ptr'Storage_Size use 0;
      package Conv is
         new Address_To_Named_Access_Conversions (C.char, char_ptr);
      Result_A : C.char_array (C.size_t);
      for Result_A'Address use Conv.To_Address (char_ptr (Result));
   begin
      Result_Length := Source'Length;
      memcpy (Result_A'Address, Source'Address, Result_Length);
      Result_A (Result_Length) := C.char'Val (0);
   end To_C;

end System.Zero_Terminated_Strings;
