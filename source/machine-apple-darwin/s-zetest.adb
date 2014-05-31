with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
package body System.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);
   use type C.size_t;

   function strlen (Item : not null access constant C.char) return C.size_t;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   procedure memcpy (s1, s2 : Address; n : C.size_t);
   pragma Import (Intrinsic, memcpy, "__builtin_memcpy");

   --  implementation

   function Value (
      First : not null access constant C.char)
      return String is
   begin
      return Value (First, strlen (First));
   end Value;

   function Value (
      First : not null access constant C.char;
      Length : C.size_t)
      return String
   is
      package Conv is
         new Address_To_Constant_Access_Conversions (
            C.char,
            C.char_const_ptr);
      Result : String (1 .. Natural (Length));
      for Result'Address use Conv.To_Address (First);
   begin
      return Result;
   end Value;

   procedure To_C (
      Source : String;
      Result : not null access constant C.char)
   is
      Dummy : C.size_t;
      pragma Unreferenced (Dummy);
   begin
      To_C (Source, Result, Dummy);
   end To_C;

   procedure To_C (
      Source : String;
      Result : not null access constant C.char;
      Result_Length : out C.size_t)
   is
      --  Source and Result should not be overlapped
      package Conv is
         new Address_To_Named_Access_Conversions (
            C.char,
            C.char_ptr);
      Result_A : C.char_array (C.size_t);
      for Result_A'Address use Conv.To_Address (Result);
   begin
      Result_Length := Source'Length;
      memcpy (
         Result_A'Address,
         Source'Address,
         Result_Length);
      Result_A (Result_Length) := C.char'Val (0);
   end To_C;

end System.Zero_Terminated_Strings;
