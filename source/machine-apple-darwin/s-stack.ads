pragma License (Unrestricted);
--  runtime unit specialized for Darwin
with C.pthread;
package System.Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address);

   procedure Fake_Return_From_Signal_Handler;

end System.Stack;
