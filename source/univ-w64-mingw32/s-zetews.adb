pragma License (Unrestricted);
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with C.string;
with C.winnls;
package body System.Zero_Terminated_WStrings is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.C_CHAR, C.winnt.LPSTR);
   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  implementation

   function Value (
      First : not null access constant C.winnt.WCHAR)
      return String is
   begin
      return Value (First, C.string.wcslen (First));
   end Value;

   function Value (
      First : not null access constant C.winnt.WCHAR;
      Length : C.size_t)
      return String
   is
      Result : String (1 .. Natural (Length) * 2);
      Result_Length : Natural;
   begin
      Result_Length := Natural (C.winnls.WideCharToMultiByte (
         C.winnls.CP_UTF8,
         0,
         First,
         C.signed_int (Length),
         LPSTR_Conv.To_Pointer (Result'Address),
         Result'Length,
         null,
         null));
      return Result (1 .. Result_Length);
   end Value;

   procedure Convert (
      Source : String;
      Result : not null access C.winnt.WCHAR)
   is
      Dummy : C.size_t;
      pragma Unreferenced (Dummy);
   begin
      Convert (Source, Result, Dummy);
   end Convert;

   procedure Convert (
      Source : String;
      Result : not null access C.winnt.WCHAR;
      Result_Length : out C.size_t)
   is
      Source_Length : constant Natural := Source'Length;
      Result_End : C.winnt.LPWSTR;
   begin
      Result_Length := C.size_t (C.winnls.MultiByteToWideChar (
         C.winnls.CP_UTF8,
         0,
         LPSTR_Conv.To_Pointer (Source'Address),
         C.signed_int (Source_Length),
         Result,
         C.signed_int (Source_Length))); -- assuming Result has enough size
      Result_End := LPWSTR_Conv.To_Pointer (
         LPWSTR_Conv.To_Address (C.winnt.LPWSTR (Result))
         + System.Storage_Elements.Storage_Offset (Result_Length)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      Result_End.all := C.winnt.WCHAR'Val (0);
   end Convert;

end System.Zero_Terminated_WStrings;
