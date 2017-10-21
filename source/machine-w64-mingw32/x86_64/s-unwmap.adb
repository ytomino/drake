pragma Check_Policy (Trace => Ignore);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Stack;
with System.Storage_Map;
with System.Unwind.Occurrences;
with System.Unwind.Standard;
with C.basetsd;
with C.vadefs;
with C.winbase;
with C.windef;
with C.winnls;
package body System.Unwind.Mapping is
   pragma Suppress (All_Checks);
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.winnt.LPSTR);
   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  implementation

   function New_Machine_Occurrence_From_SEH (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr)
      return Representation.Machine_Occurrence_Access
   is
      function Cast is
         new Ada.Unchecked_Conversion (C.windef.HMODULE, C.windef.LPCVOID);
      function Cast is
         new Ada.Unchecked_Conversion (C.winnt.LPWSTR_ptr, C.winnt.LPTSTR);
      function Cast is
         new Ada.Unchecked_Conversion (
            C.basetsd.ULONG_PTR_ptr,
            C.vadefs.va_list_ptr);
      Code : constant C.windef.DWORD := Exception_Record.ExceptionCode;
      Eexception_Id : Exception_Data_Access;
      Stack_Guard : Address := Null_Address;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      case Code is
         when C.winbase.EXCEPTION_ACCESS_VIOLATION => -- 0xC0000005
            --  get stack range
            declare
               Stack_Top, Stack_Bottom : Address;
               AV_Address : constant Address :=
                  System'To_Address (
                     Exception_Record.ExceptionInformation (1));
            begin
               Stack.Get (Top => Stack_Top, Bottom => Stack_Bottom);
               if AV_Address >= Stack_Top - 4096
                  and then AV_Address < Stack_Bottom
               then -- stack overflow
                  Stack_Guard := Stack_Top + 4096;
                  Eexception_Id := Standard.Storage_Error'Access;
               else
                  Eexception_Id := Standard.Program_Error'Access;
               end if;
            end;
         when C.winbase.EXCEPTION_INVALID_HANDLE => -- 0xC0000008
            Eexception_Id := Standard.Constraint_Error'Access;
         when C.winbase.EXCEPTION_ARRAY_BOUNDS_EXCEEDED -- 0xC000008C
            | C.winbase.EXCEPTION_FLT_DENORMAL_OPERAND -- 0xC000008D
            | C.winbase.EXCEPTION_FLT_DIVIDE_BY_ZERO -- 0xC000008E
            | C.winbase.EXCEPTION_FLT_INEXACT_RESULT -- 0xC000008F
            | C.winbase.EXCEPTION_FLT_INVALID_OPERATION -- 0xC0000090
            | C.winbase.EXCEPTION_FLT_OVERFLOW -- 0xC0000091
            | C.winbase.EXCEPTION_FLT_STACK_CHECK -- 0xC0000092
            | C.winbase.EXCEPTION_FLT_UNDERFLOW -- 0xC0000093
            | C.winbase.EXCEPTION_INT_DIVIDE_BY_ZERO -- 0xC0000094
            | C.winbase.EXCEPTION_INT_OVERFLOW => -- 0xC0000095
            Eexception_Id := Standard.Constraint_Error'Access;
         when C.winbase.EXCEPTION_STACK_OVERFLOW => -- 0xC00000FD
            --  get stack range
            declare
               Dummy : Address;
            begin
               Stack.Get (Top => Stack_Guard, Bottom => Dummy);
            end;
            Stack_Guard := Stack_Guard + 4096;
            --  Storage_Error
            Eexception_Id := Standard.Storage_Error'Access;
         when others =>
            Eexception_Id := null;
      end case;
      if Eexception_Id /= null then
         declare
            Message : String (1 .. 256);
            Message_Last : Natural;
            Wide_Message : aliased C.winnt.LPWSTR;
            Wide_Message_Length : C.windef.DWORD;
         begin
            Wide_Message_Length := C.winbase.FormatMessage (
               dwFlags =>
                  C.winbase.FORMAT_MESSAGE_FROM_HMODULE
                  or C.winbase.FORMAT_MESSAGE_ARGUMENT_ARRAY
                  or C.winbase.FORMAT_MESSAGE_ALLOCATE_BUFFER,
               lpSource => Cast (Storage_Map.NTDLL),
               dwMessageId => Code,
               dwLanguageId => C.winnt.LANG_USER_DEFAULT,
               lpBuffer => Cast (Wide_Message'Unchecked_Access),
               nSize => 0,
               Arguments =>
                  Cast (Exception_Record.ExceptionInformation (0)'Access));
            declare
               Wide_Message_Last : constant Natural :=
                  Integer (Wide_Message_Length);
               Wide_Message_All : Wide_String (1 .. Wide_Message_Last);
               for Wide_Message_All'Address use Wide_Message.all'Address;
            begin
               if Wide_Message_All (
                        Wide_Message_Last - 2 .. Wide_Message_Last) =
                     """0x"
                  and then Code = C.winbase.EXCEPTION_ACCESS_VIOLATION
               then
                  --  bug of FormatString ???
                  --  there are some reports in stackoverflow.com
                  --  perhaps, FormatString can not convert %p in
                  --  'The instruction at %p referenced memory at %p.'
                  Message (1 .. 21) := "The instruction at 0x";
                  Message_Last := 21;
                  Formatting.Address.Image (
                     Address (Exception_Record.ExceptionAddress),
                     Message (
                        Message_Last + 1 ..
                        Message'Last
                           + Formatting.Address.Address_String'Length),
                     Set => Formatting.Lower_Case);
                  Message_Last :=
                     Message_Last + Formatting.Address.Address_String'Length;
                  Message (Message_Last + 1 .. Message_Last + 24) :=
                     " referenced memory at 0x";
                  Message_Last := Message_Last + 24;
                  Formatting.Address.Image (
                     System'To_Address (
                        Exception_Record.ExceptionInformation (1)),
                     Message (
                        Message_Last + 1 ..
                        Message_Last
                           + Formatting.Address.Address_String'Length),
                     Set => Formatting.Lower_Case);
                  Message_Last :=
                     Message_Last + Formatting.Address.Address_String'Length;
                  Message (Message_Last + 1) := '.';
                  Message_Last := Message_Last + 1;
               else
                  Message_Last := Natural (
                     C.winnls.WideCharToMultiByte (
                        C.winnls.CP_UTF8,
                        0,
                        Wide_Message,
                        C.signed_int (Wide_Message_Length),
                        LPSTR_Conv.To_Pointer (Message'Address),
                        Message'Length,
                        null,
                        null));
               end if;
            end;
            declare
               Dummy : C.windef.HLOCAL;
            begin
               Dummy := C.winbase.LocalFree (
                  C.windef.HLOCAL (LPWSTR_Conv.To_Address (Wide_Message)));
            end;
            declare
               Result : constant
                     not null Representation.Machine_Occurrence_Access :=
                  Occurrences.New_Machine_Occurrence (
                     Stack_Guard => Stack_Guard);
            begin
               Occurrences.Set_Exception_Message (
                  Id => Eexception_Id,
                  Message => Message,
                  X => Result.Occurrence);
               Occurrences.Backtrace (Result.Occurrence);
               pragma Check (Trace, Ada.Debug.Put ("leave, mapped"));
               return Result;
            end;
         end;
      else
         pragma Check (Trace, Ada.Debug.Put ("leave, unmapped!"));
         return null; -- would not be handled in Ada
      end if;
   end New_Machine_Occurrence_From_SEH;

end System.Unwind.Mapping;
