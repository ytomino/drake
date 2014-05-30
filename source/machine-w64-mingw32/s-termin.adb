with System.Address_To_Named_Access_Conversions;
with C.stdlib;
with C.winbase;
with C.windef;
with C.winnls;
with C.winnt;
package body System.Termination is
   pragma Suppress (All_Checks);

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.C_CHAR, C.winnt.LPSTR);
   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  implementation

   procedure Error_Put (S : String) is
      Written : aliased C.windef.DWORD;
      Dummy : C.windef.WINBOOL;
      pragma Unreferenced (Dummy);
      S16 : Wide_String (1 .. S'Length);
      S16_Last : Natural;
      SL : String (1 .. S16'Length * 2);
      SL_Last : Natural;
   begin
      --  convert S that is UTF-8 to active codepage
      S16_Last := Natural (C.winnls.MultiByteToWideChar (
         C.winnls.CP_UTF8,
         0,
         LPSTR_Conv.To_Pointer (S'Address),
         S'Length,
         LPWSTR_Conv.To_Pointer (S16'Address),
         S16'Length));
      SL_Last := Natural (C.winnls.WideCharToMultiByte (
         C.winnls.CP_ACP,
         0,
         LPWSTR_Conv.To_Pointer (S16'Address),
         C.signed_int (S16_Last),
         LPSTR_Conv.To_Pointer (SL'Address),
         SL'Length,
         null,
         null));
      --  output
      Dummy := C.winbase.WriteFile (
         C.winbase.GetStdHandle (C.winbase.STD_ERROR_HANDLE),
         C.windef.LPCVOID (SL'Address),
         C.windef.DWORD (SL_Last),
         Written'Access,
         null);
   end Error_Put;

   procedure Error_New_Line is
   begin
      Error_Put ((1 => Character'Val (10), 2 => Character'Val (13)));
   end Error_New_Line;

   procedure Force_Abort is
   begin
      C.stdlib.C_abort;
   end Force_Abort;

   procedure Register_Exit (Handler : not null Exit_Handler) is
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      --  atexit requires handler that has C calling-convention,
      --  but Ada procedure having no argument is same as C.
      Dummy := C.stdlib.atexit (Handler);
   end Register_Exit;

end System.Termination;
