with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with C.string;
with C.winnls;
package body System.Zero_Terminated_WStrings is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.C_CHAR, C.winnt.LPSTR);

   --  implementation

   function Value (Item : not null access constant C.winnt.WCHAR)
      return String is
   begin
      return Value (Item, C.string.wcslen (Item));
   end Value;

   function Value (
      Item : not null access constant C.winnt.WCHAR;
      Length : C.size_t)
      return String
   is
      Result : String (1 .. Natural (Length) * 2);
      Result_Length : C.signed_int;
   begin
      Result_Length :=
         C.winnls.WideCharToMultiByte (
            C.winnls.CP_UTF8,
            0,
            Item,
            C.signed_int (Length),
            LPSTR_Conv.To_Pointer (Result'Address),
            Result'Length,
            null,
            null);
      return Result (1 .. Natural (Result_Length));
   end Value;

   procedure To_C (Source : String; Result : not null access C.winnt.WCHAR) is
      Dummy : C.size_t;
   begin
      To_C (Source, Result, Dummy);
   end To_C;

   procedure To_C (
      Source : String;
      Result : not null access C.winnt.WCHAR;
      Result_Length : out C.size_t)
   is
      type LPWSTR is access all C.winnt.WCHAR -- local type
         with Convention => C;
      for LPWSTR'Storage_Size use 0;
      package LPWSTR_Conv is
         new Address_To_Named_Access_Conversions (C.winnt.WCHAR, LPWSTR);
      Source_Length : constant Natural := Source'Length;
      Raw_Result_Length : C.signed_int;
      Result_End : LPWSTR;
   begin
      Raw_Result_Length :=
         C.winnls.MultiByteToWideChar (
            C.winnls.CP_UTF8,
            0,
            LPSTR_Conv.To_Pointer (Source'Address),
            C.signed_int (Source_Length),
            Result,
            C.signed_int (Source_Length)); -- assuming Result has enough size
      Result_End :=
         LPWSTR_Conv.To_Pointer (
            LPWSTR_Conv.To_Address (LPWSTR (Result))
               + Storage_Elements.Storage_Offset (Raw_Result_Length)
                  * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      Result_End.all := C.winnt.WCHAR'Val (0);
      Result_Length := C.size_t (Raw_Result_Length);
   end To_C;

end System.Zero_Terminated_WStrings;
