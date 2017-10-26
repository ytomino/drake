pragma License (Unrestricted);
--  runtime unit specialized for FreeBSD
with C.pthread;
with C.signal;
package System.Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address);

   procedure Fake_Return_From_Signal_Handler is null;
      --  FreeBSD does not have UC_RESET_ALT_STACK ?

   function Fault_Address (Info : C.signal.siginfo_t) return Address;
   pragma Inline (Fault_Address);

end System.Stack;
