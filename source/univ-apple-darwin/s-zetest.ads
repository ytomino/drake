pragma License (Unrestricted);
--  implementation unit
with C;
package System.Zero_Terminated_Strings is
   pragma Preelaborate;

   function Value (
      First : not null access constant C.char)
      return String;
   function Value (
      First : not null access constant C.char;
      Length : C.size_t)
      return String;

   procedure To_C (
      Source : String;
      Result : not null access constant C.char);
   procedure To_C (
      Source : String;
      Result : not null access constant C.char;
      Result_Length : out C.size_t);

   Expanding : constant := 1; -- UTF-8 to UTF-8

end System.Zero_Terminated_Strings;
