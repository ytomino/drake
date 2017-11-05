with Ada.Unchecked_Conversion;
with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Stack;
with System.Unwind.Raising;
with System.Unwind.Standard;
with C.string;
with C.sys.ucontext;
package body System.Unwind.Mapping is
   pragma Suppress (All_Checks);
   use type C.signed_int;
   use type C.signed_long;
   use type C.unsigned_int;
   use type C.unsigned_long;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   procedure sigaction_Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
      with Convention => C;
   pragma No_Return (sigaction_Handler);

   procedure sigaction_Handler (
      Signal_Number : C.signed_int;
      Info : access C.signal.siginfo_t;
      Context : C.void_ptr)
   is
      --  allow to inspect the parameters with debugger.
      type ucontext_t_ptr is access constant C.sys.ucontext.ucontext_t
         with Convention => C;
      for ucontext_t_ptr'Storage_Size use 0;
      package ucontext_t_ptr_Conv is
         new Address_To_Constant_Access_Conversions (
            C.sys.ucontext.ucontext_t,
            ucontext_t_ptr);
      uap : constant ucontext_t_ptr :=
         ucontext_t_ptr_Conv.To_Pointer (Address (Context));
      pragma Inspection_Point (Signal_Number);
      pragma Inspection_Point (Info);
      pragma Inspection_Point (uap);
      --  space for overflow detection, decided for CB1010C
      Stack_Overflow_Space : constant :=
         (Standard'Address_Size / Standard'Storage_Unit) * 8 * 1024 * 1024;
      --  the components of the exception.
      Message : constant C.char_ptr := C.string.strsignal (Signal_Number);
      Message_Length : constant C.size_t := strlen (Message);
      Eexception_Id : Exception_Data_Access;
      Stack_Guard : Address := Null_Address;
   begin
      case Signal_Number is
         when C.signal.SIGFPE =>
            Eexception_Id := Standard.Constraint_Error'Access;
         when C.signal.SIGBUS | C.signal.SIGSEGV =>
            declare
               Top, Bottom : Address;
               Fault : Address;
            begin
               Stack.Get (Top => Top, Bottom => Bottom);
               Stack.Fake_Return_From_Signal_Handler;
               if Info /= null then
                  Fault := Stack.Fault_Address (Info.all);
               else
                  Fault := Null_Address;
               end if;
               if Fault /= Null_Address
                  and then Fault >= Top - Stack_Overflow_Space
                  and then Fault < Bottom
               then -- stack overflow
                  Stack_Guard := Top + C.signal.MINSIGSTKSZ;
                  Eexception_Id := Standard.Storage_Error'Access;
               else
                  Eexception_Id := Standard.Program_Error'Access;
               end if;
            end;
         when others =>
            Eexception_Id := Standard.Program_Error'Access;
      end case;
      declare
         Message_All : String (1 .. Integer (Message_Length));
         for Message_All'Address use char_ptr_Conv.To_Address (Message);
      begin
         Raising.Raise_From_Signal_Handler (
            Eexception_Id,
            Message => Message_All,
            Stack_Guard => Stack_Guard);
      end;
   end sigaction_Handler;

   procedure Set_Signal_Stack (S : access Signal_Stack_Type);
   procedure Set_Signal_Stack (S : access Signal_Stack_Type) is
      function Cast is
         new Ada.Unchecked_Conversion (C.char_ptr, C.void_ptr); -- OSX
      function Cast is
         new Ada.Unchecked_Conversion (C.char_ptr, C.char_ptr); -- FreeBSD
      pragma Warnings (Off, Cast);
      stack : aliased C.signal.stack_t := (
         ss_sp => Cast (S (S'First)'Access),
         ss_size => Signal_Stack_Type'Size / Standard'Storage_Unit,
         ss_flags => 0);
      Dummy : C.signed_int;
   begin
      Dummy := C.signal.sigaltstack (stack'Access, null);
   end Set_Signal_Stack;

   Signal_Stack : aliased Signal_Stack_Type;

   --  implementation

   procedure Install_Exception_Handler (SEH : Address) is
      pragma Unreferenced (SEH);
      act : aliased C.signal.struct_sigaction := (
         (Unchecked_Tag => 1, sa_sigaction => sigaction_Handler'Access),
         others => <>);
      Dummy : C.signed_int;
   begin
      act.sa_flags := C.signed_int (
         C.unsigned_int'(C.signal.SA_NODEFER or C.signal.SA_SIGINFO));
      Dummy := C.signal.sigemptyset (act.sa_mask'Access);
      --  illegal instruction
      Dummy := C.signal.sigaction (C.signal.SIGILL, act'Access, null);
      --  floating-point exception
      Dummy := C.signal.sigaction (C.signal.SIGFPE, act'Access, null);
      --  bus error
      Set_Signal_Stack (Signal_Stack'Access);
      act.sa_flags := C.signed_int (C.unsigned_int (act.sa_flags)
         or C.signal.SA_ONSTACK);
      Dummy := C.signal.sigaction (C.signal.SIGBUS, act'Access, null);
      --  segmentation violation
      Dummy := C.signal.sigaction (C.signal.SIGSEGV, act'Access, null);
   end Install_Exception_Handler;

   procedure Install_Task_Exception_Handler (
      SEH : Address;
      Signal_Stack : not null access Signal_Stack_Type)
   is
      pragma Unreferenced (SEH);
   begin
      --  sigaction setting would be inherited from environment task.
      Set_Signal_Stack (Signal_Stack);
   end Install_Task_Exception_Handler;

end System.Unwind.Mapping;
