with System.Debug;
with System.Storage_Elements;
with C.sys.syscall;
with C.unistd;
package body System.Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address) is
   begin
      Bottom := Address (C.pthread.pthread_get_stackaddr_np (Thread));
      Top :=
         Bottom
         - Storage_Elements.Storage_Offset (
            C.pthread.pthread_get_stacksize_np (Thread));
   end Get;

   procedure Fake_Return_From_Signal_Handler is
      UC_RESET_ALT_STACK : constant := 16#80000000#; -- ???
      R : C.signed_int;
   begin
      --  emulate normal return
      R := C.unistd.syscall (
         C.sys.syscall.SYS_sigreturn,
         C.void_ptr (Null_Address),
         UC_RESET_ALT_STACK);
      pragma Check (Debug,
         Check =>
            not (R < 0)
            or else Debug.Runtime_Error (
               "syscall (SYS_sigreturn, ...) failed"));
   end Fake_Return_From_Signal_Handler;

   function Fault_Address (Info : C.signal.siginfo_t) return Address is
   begin
      return Info.si_addr;
   end Fault_Address;

end System.Stack;
