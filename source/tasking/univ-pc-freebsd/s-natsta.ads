pragma License (Unrestricted);
--  implementation unit
with System.Storage_Elements;
with C.pthread;
package System.Native_Stack is
   pragma Preelaborate;

   procedure Get (
      Thread : C.pthread.pthread_t;
      Addr : out Address;
      Size : out Storage_Elements.Storage_Count);

end System.Native_Stack;
