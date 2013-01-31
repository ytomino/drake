pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address_Image;
with System.Native_Stack;
with System.Unwind.Raising;
with System.Unwind.Standard;
with C.basetsd;
with C.excpt;
with C.stdlib;
with C.vadefs;
with C.winbase;
with C.windef;
with C.winnls;
with C.winnt;
package body System.Termination is
   pragma Suppress (All_Checks);
   use type Unwind.Exception_Data_Access;
   use type C.size_t;
   use type C.wchar_t_array;
   use type C.windef.DWORD;
   use type C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr;

   package LPSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.C_CHAR, C.winnt.LPSTR);
   package LPWSTR_Conv is
      new Address_To_Named_Access_Conversions (C.winnt.WCHAR, C.winnt.LPWSTR);

   type SEH_Handler is access function (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr;
      Establisher_Frame : C.void_ptr;
      Context_Record : C.winnt.struct_CONTEXT_ptr;
      Dispatcher_Context : C.void_ptr)
      return C.signed_int;
   pragma Convention (C, SEH_Handler);
   type SEH_Record is record
      ExceptionList : aliased C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr;
      Handler : aliased SEH_Handler;
   end record;
   for SEH_Record'Size use Standard'Address_Size * 2;
   pragma Suppress_Initialization (SEH_Record);

   function Ada_SEH_Handler (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr;
      Establisher_Frame : C.void_ptr;
      Context_Record : C.winnt.struct_CONTEXT_ptr;
      Dispatcher_Context : C.void_ptr)
      return C.signed_int;
   pragma Convention (C, Ada_SEH_Handler);

   SEH_In_Main : C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr;

   A_NTDLL_DLL : aliased constant Wide_String :=
      "NTDLL.DLL" & Wide_Character'Val (0);
   C_NTDLL_DLL : aliased C.wchar_t_array (0 .. A_NTDLL_DLL'Length - 1);
   for C_NTDLL_DLL'Address use A_NTDLL_DLL'Address;

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

   function Ada_SEH_Handler (
      Exception_Record : C.winnt.struct_EXCEPTION_RECORD_ptr;
      Establisher_Frame : C.void_ptr;
      Context_Record : C.winnt.struct_CONTEXT_ptr;
      Dispatcher_Context : C.void_ptr)
      return C.signed_int
   is
      function Cast is new Ada.Unchecked_Conversion (
         C.windef.HMODULE,
         C.windef.LPCVOID);
      function Cast is new Ada.Unchecked_Conversion (
         C.winnt.LPWSTR_ptr,
         C.winnt.LPTSTR);
      function Cast is new Ada.Unchecked_Conversion (
         C.basetsd.ULONG_PTR_ptr,
         C.vadefs.va_list_ptr);
      pragma Unreferenced (Establisher_Frame);
      pragma Unreferenced (Context_Record);
      pragma Unreferenced (Dispatcher_Context);
      Code : constant C.windef.DWORD := Exception_Record.ExceptionCode;
      Eexception_Id : Unwind.Exception_Data_Access;
      Stack_Guard : Address := Null_Address;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      case Code is
         when C.winbase.EXCEPTION_ACCESS_VIOLATION => -- 0xC0000005
            --  get stack range
            declare
               Stack_Top, Stack_Bottom : Address;
               AV_Address : constant Address :=
                  Address (Exception_Record.ExceptionInformation (1));
            begin
               Native_Stack.Get (Top => Stack_Top, Bottom => Stack_Bottom);
               if AV_Address >= Stack_Top - 4096
                  and then AV_Address < Stack_Bottom
               then -- stack overflow
                  Stack_Guard := Stack_Top + 4096;
                  Eexception_Id := Unwind.Standard.Storage_Error'Access;
               else
                  Eexception_Id := Unwind.Standard.Program_Error'Access;
               end if;
            end;
         when C.winbase.EXCEPTION_INVALID_HANDLE => -- 0xC0000008
            Eexception_Id := Unwind.Standard.Constraint_Error'Access;
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
            Eexception_Id := Unwind.Standard.Constraint_Error'Access;
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
            Eexception_Id := Unwind.Standard.Storage_Error'Access;
         when others =>
            Eexception_Id := null;
      end case;
      if Eexception_Id /= null then
         declare
            NTDLL_Handle : constant C.windef.HMODULE :=
               C.winbase.GetModuleHandle (C_NTDLL_DLL (0)'Access);
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
               if Wide_Message (Wide_Message_Last - 2 .. Wide_Message_Last)
                  = """0x"
                  and then Code = C.winbase.EXCEPTION_ACCESS_VIOLATION
               then
                  --  bug of FormatString ???
                  --  there are some reports in stackoverflow.com
                  --  perhaps, FormatString can not convert %p in
                  --  'The instruction at %p referenced memory at %p.'
                  Message (1 .. 21) := "The instruction at 0x";
                  Message_Last := 21;
                  Formatting.Address_Image (
                     Message (Message_Last + 1 .. Message'Last),
                     Message_Last,
                     Address (
--                      Exception_Record.ExceptionInformation (0)),
                        Exception_Record.ExceptionAddress),
                     Set => Formatting.Lower_Case);
                  Message (Message_Last + 1 .. Message_Last + 24) :=
                     " referenced memory at 0x";
                  Message_Last := Message_Last + 24;
                  Formatting.Address_Image (
                     Message (Message_Last + 1 .. Message'Last),
                     Message_Last,
                     Address (
                        Exception_Record.ExceptionInformation (1)),
                     Set => Formatting.Lower_Case);
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
            pragma Check (Trace, Ada.Debug.Put ("raising"));
            Unwind.Raising.Raise_From_Signal_Handler (
               Eexception_Id,
               Message => Message (1 .. Message_Last),
               Stack_Guard => Stack_Guard);
         end;
      else
         pragma Check (Trace, Ada.Debug.Put ("leave"));
         return C.excpt.ExceptionContinueSearch;
      end if;
   end Ada_SEH_Handler;

   procedure Install_Exception_Handler (SEH : Address) is
      function Cast is new Ada.Unchecked_Conversion (
         C.winnt.struct_TEB_ptr,
         C.winnt.NT_TIB_ptr);
      function Cast is new Ada.Unchecked_Conversion (
         Address,
         C.winnt.struct_EXCEPTION_REGISTRATION_RECORD_ptr);
      TEB : constant C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      TIB : constant C.winnt.NT_TIB_ptr := Cast (TEB);
      SEH_Rec : SEH_Record;
      for SEH_Rec'Address use SEH;
   begin
      SEH_Rec.ExceptionList := TIB.ExceptionList;
      SEH_Rec.Handler := Ada_SEH_Handler'Access;
      --  register new handler
      TIB.ExceptionList := Cast (SEH);
      --  memory the stack area of main
      SEH_In_Main := Cast (SEH);
      --  note, raising an exception from SetUnhandledExceptionFilter is
      --    probably too late
   end Install_Exception_Handler;

   procedure Reinstall_Exception_Handler is
      function Cast is new Ada.Unchecked_Conversion (
         C.winnt.struct_TEB_ptr,
         C.winnt.NT_TIB_ptr);
      TEB : constant C.winnt.struct_TEB_ptr := C.winnt.NtCurrentTeb;
      TIB : constant C.winnt.NT_TIB_ptr := Cast (TEB);
   begin
      if TIB.ExceptionList /= SEH_In_Main then
         TIB.ExceptionList := SEH_In_Main;
         pragma Check (Trace, Ada.Debug.Put ("reinstalled"));
      end if;
   end Reinstall_Exception_Handler;

end System.Termination;
