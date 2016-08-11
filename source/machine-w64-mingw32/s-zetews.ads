pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.winnt;
package System.Zero_Terminated_WStrings is
   pragma Preelaborate;
   --  This package targets at not Wide_String in Ada, not wchar_t in C
   --  but LPWSTR in Windows

   --  convert to UTF-8 from a first address of zero-terminated wide string
   function Value (First : not null access constant C.winnt.WCHAR)
      return String;
   function Value (
      First : not null access constant C.winnt.WCHAR;
      Length : C.size_t)
      return String;

   --  convert to zero-terminated wide string from UTF-8
   procedure To_C (Source : String; Result : not null access C.winnt.WCHAR);
   procedure To_C (
      Source : String;
      Result : not null access C.winnt.WCHAR;
      Result_Length : out C.size_t);

   Expanding : constant := 1; -- same as Expanding_From_8_To_16

end System.Zero_Terminated_WStrings;
