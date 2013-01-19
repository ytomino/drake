with Ada.Unchecked_Conversion;
package body System.Native_Stack is
   pragma Suppress (All_Checks);

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address)
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      Bottom := Cast (C.pthread.pthread_get_stackaddr_np (Thread));
      Top := Bottom - Address (C.pthread.pthread_get_stacksize_np (Thread));
   end Get;

end System.Native_Stack;
