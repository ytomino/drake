with Ada.Unchecked_Conversion;
package body System.Native_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   procedure Get (
      Thread : C.pthread.pthread_t;
      Addr : out Address;
      Size : out Storage_Elements.Storage_Count)
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      Size := Storage_Elements.Storage_Count (
         C.pthread.pthread_get_stacksize_np (Thread));
      Addr := Cast (C.pthread.pthread_get_stackaddr_np (Thread)) - Size;
   end Get;

end System.Native_Stack;
