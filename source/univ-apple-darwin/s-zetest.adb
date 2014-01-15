with System.Address_To_Constant_Access_Conversions;
package body System.Zero_Terminated_Strings is
   pragma Suppress (All_Checks);

   function strlen (Item : not null access constant C.char) return C.size_t;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

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

end System.Zero_Terminated_Strings;
