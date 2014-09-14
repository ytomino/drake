pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Native_Stack;
with System.Unwind.Raising;
with System.Unwind.Standard;
with C.basetsd;
with C.vadefs;
with C.winbase;
with C.windef;
with C.winnls;
package body System.Unwind.Mapping is
   pragma Suppress (All_Checks);
   use type Exception_Data_Access;
   use type C.size_t;
   use type C.wchar_t_array;
   use type C.windef.DWORD;
   use type C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.C_CHAR, C.winnt.LPSTR);
   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   --  weak reference for System.Unwind.Tracebacks (ELF only ?)
   Call_Chain : access procedure (
      Current : not null Exception_Occurrence_Access);
   pragma Import (Ada, Call_Chain, "__drake_ref_call_chain");
   pragma Weak_External (Call_Chain);

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
               Native_Stack.Get (Top => Stack_Top, Bottom => Stack_Bottom);
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
            | C.winbase.EXCEPTION_INT_OVERFLOW -- 0xC0000095
         =>
            Eexception_Id := Standard.Constraint_Error'Access;
         when C.winbase.EXCEPTION_STACK_OVERFLOW => -- 0xC00000FD
            --  get stack range
            declare
               Dummy : Address;
               pragma Unreferenced (Dummy);
            begin
               Native_Stack.Get (Top => Stack_Guard, Bottom => Dummy);
               Stack_Guard := Stack_Guard + 4096;
            end;
            --  Storage_Error
            Eexception_Id := Standard.Storage_Error'Access;
         when others =>
            Eexception_Id := null;
      end case;
      if Eexception_Id /= null then
         declare
            NTDLL_Handle : constant C.windef.HMODULE := Native_Stack.NTDLL;
            Message : String (1 .. 256);
            Message_Last : Natural;
            C_Wide_Buf : aliased C.winnt.LPWSTR;
            R : C.windef.DWORD;
            Dummy : C.windef.HLOCAL;
            pragma Unreferenced (Dummy);
         begin
            R := C.winbase.FormatMessage (
               dwFlags => C.winbase.FORMAT_MESSAGE_FROM_HMODULE
                  or C.winbase.FORMAT_MESSAGE_ARGUMENT_ARRAY
                  or C.winbase.FORMAT_MESSAGE_ALLOCATE_BUFFER,
               lpSource => Cast (NTDLL_Handle),
               dwMessageId => Code,
               dwLanguageId => C.winnt.LANG_USER_DEFAULT,
               lpBuffer => Cast (C_Wide_Buf'Unchecked_Access),
               nSize => 0,
               Arguments => Cast (
                  Exception_Record.ExceptionInformation (0)'Access));
            declare
               Wide_Message : Wide_String (Positive);
               for Wide_Message'Address use C_Wide_Buf.all'Address;
               Wide_Message_Last : constant Natural := Natural (R);
            begin
               if Wide_Message (Wide_Message_Last - 2 .. Wide_Message_Last) =
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
                  Message_Last := Natural (C.winnls.WideCharToMultiByte (
                     C.winnls.CP_UTF8,
                     0,
                     LPWSTR_Conv.To_Pointer (Wide_Message'Address),
                     C.signed_int (Wide_Message_Last),
                     LPSTR_Conv.To_Pointer (Message'Address),
                     Message'Length,
                     null,
                     null));
               end if;
            end;
            Dummy := C.winbase.LocalFree (
               C.windef.HLOCAL (LPWSTR_Conv.To_Address (C_Wide_Buf)));
            declare
               Result : constant
                  not null Representation.Machine_Occurrence_Access :=
                  Representation.New_Machine_Occurrence;
            begin
               Result.Stack_Guard := Stack_Guard;
               Raising.Set_Exception_Message (
                  Id => Eexception_Id,
                  Message => Message,
                  X => Result.Occurrence);
               if Call_Chain'Address /= Null_Address then
                  Call_Chain (Result.Occurrence'Access);
                  declare
                     function Report return Boolean;
                     function Report return Boolean is
                     begin
                        Raising.Report_Traceback (Result.Occurrence);
                        return True;
                     end Report;
                  begin
                     pragma Check (Trace, Ada.Debug.Put ("info..."));
                     pragma Check (Trace, Report);
                  end;
               end if;
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
