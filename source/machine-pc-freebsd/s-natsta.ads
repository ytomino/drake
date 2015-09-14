pragma License (Unrestricted);
--  runtime unit specialized for FreeBSD
with C.pthread;
package System.Native_Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address);

   procedure Fake_Return_From_Signal_Handler is null;
      --  FreeBSD does not have UC_RESET_ALT_STACK ?

end System.Native_Stack;
