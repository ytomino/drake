with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with C.sys.syscall;
with C.unistd;
package body System.Native_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address)
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      Bottom := Cast (C.pthread.pthread_get_stackaddr_np (Thread));
      Top :=
         Bottom
         - Storage_Elements.Storage_Offset (
            C.pthread.pthread_get_stacksize_np (Thread));
   end Get;

   procedure Fake_Return_From_Signal_Handler is
      UC_RESET_ALT_STACK : constant := 16#80000000#; -- ???
   begin
      --  emulate normal return
      if C.unistd.syscall (
         C.sys.syscall.SYS_sigreturn,
         C.void_ptr (Null_Address),
         UC_RESET_ALT_STACK) < 0
      then
         null; -- no error handling
      end if;
   end Fake_Return_From_Signal_Handler;

end System.Native_Stack;
