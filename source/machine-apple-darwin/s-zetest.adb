with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
package body System.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);
   use type C.size_t;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   procedure memcpy (s1, s2 : Address; n : C.size_t)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   --  implementation

   function Value (Item : not null access constant C.char) return String is
   begin
      return Value (Item, strlen (Item));
   end Value;

   function Value (Item : not null access constant C.char; Length : C.size_t)
      return String
   is
      type char_const_ptr is access constant C.char -- local type
         with Convention => C;
      for char_const_ptr'Storage_Size use 0;
      package Conv is
         new Address_To_Constant_Access_Conversions (C.char, char_const_ptr);
      Item_All : String (1 .. Natural (Length));
      for Item_All'Address use Conv.To_Address (char_const_ptr (Item));
   begin
      return Item_All;
   end Value;

   procedure To_C (Source : String; Result : not null access C.char) is
      Dummy : C.size_t;
   begin
      To_C (Source, Result, Dummy);
   end To_C;

   procedure To_C (
      Source : String;
      Result : not null access C.char;
      Result_Length : out C.size_t) is
   begin
      --  Source and Result should not be overlapped
      Result_Length := Source'Length;
      declare
         type char_ptr is access all C.char -- local type
            with Convention => C;
         for char_ptr'Storage_Size use 0;
         package Conv is
            new Address_To_Named_Access_Conversions (C.char, char_ptr);
         Result_All : aliased C.char_array (0 .. Result_Length);
         for Result_All'Address use Conv.To_Address (char_ptr (Result));
      begin
         memcpy (Result_All'Address, Source'Address, Result_Length);
         Result_All (Result_Length) := C.char'Val (0);
      end;
   end To_C;

end System.Zero_Terminated_Strings;
