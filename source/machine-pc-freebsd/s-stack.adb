with System.Debug;
with System.Storage_Elements;
with C.pthread_np;
package body System.Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address)
   is
      Attr : aliased C.pthread.pthread_attr_t;
      C_Addr : aliased C.void_ptr;
      C_Size : aliased C.size_t;
      OK : Boolean := False;
   begin
      if C.pthread.pthread_attr_init (Attr'Access) = 0 then
         OK := C.pthread_np.pthread_attr_get_np (Thread, Attr'Access) = 0
            and then C.pthread.pthread_attr_getstack (
               Attr'Access,
               C_Addr'Access,
               C_Size'Access) = 0;
         declare
            R : C.signed_int;
         begin
            R := C.pthread.pthread_attr_destroy (Attr'Access);
            pragma Check (Debug,
               Check =>
                  R = 0
                  or else Debug.Runtime_Error ("pthread_attr_destroy failed"));
         end;
      end if;
      if not OK then
         Top := Null_Address;
         Bottom := Null_Address;
      else
         Top := Address (C_Addr);
         Bottom := Top + Storage_Elements.Storage_Offset (C_Size);
      end if;
   end Get;

end System.Stack;
