pragma License (Unrestricted);
--  runtime unit
with C.pthread;
package System.Native_Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address);

end System.Native_Stack;
