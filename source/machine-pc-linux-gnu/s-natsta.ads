pragma License (Unrestricted);
--  runtime unit
with C.pthread;
package System.Native_Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address);

   procedure Fake_Return_From_Signal_Handler is null;
   --  Linux does not have UC_RESET_ALT_STACK,
   --  and 64bit Linux does not have SYS_sigreturn

end System.Native_Stack;
