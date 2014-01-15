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

end System.Zero_Terminated_Strings;
