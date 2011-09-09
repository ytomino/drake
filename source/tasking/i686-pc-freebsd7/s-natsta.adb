with Ada.Unchecked_Conversion;
with C.pthread_np;
package body System.Native_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure Get (
      Thread : C.pthread.pthread_t;
      Addr : out Address;
      Size : out Storage_Elements.Storage_Count)
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
      Attr : aliased C.pthread.pthread_attr_t;
      C_Addr : aliased C.void_ptr;
      C_Size : aliased C.size_t;
      OK : Boolean := False;
      Dummy : C.signed_int;
      pragma Unreferenced (Dummy);
   begin
      if C.pthread.pthread_attr_init (Attr'Access) = 0 then
         OK := C.pthread_np.pthread_attr_get_np (Thread, Attr'Access) = 0
            and then C.pthread.pthread_attr_getstackaddr (
               Attr'Access,
               C_Addr'Access) = 0
            and then C.pthread.pthread_attr_getstacksize (
               Attr'Access,
               C_Size'Access) = 0;
         Dummy := C.pthread.pthread_attr_destroy (Attr'Access);
      end if;
      if not OK then
         Addr := Null_Address;
         Size := 0;
      else
         Addr := Cast (C_Addr);
         Size := Storage_Elements.Storage_Count (C_Size);
      end if;
   end Get;

end System.Native_Stack;
