with Ada.Unchecked_Conversion;
with System.Storage_Elements;
package body System.Native_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.signed_int;

   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity);
   pragma Import (Ada, Runtime_Error, "__drake_runtime_error");

   procedure Get (
      Thread : C.pthread.pthread_t := C.pthread.pthread_self;
      Top, Bottom : out Address)
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
      Attr : aliased C.pthread.pthread_attr_t;
      C_Addr : aliased C.void_ptr;
      C_Size : aliased C.size_t;
      OK : Boolean := False;
      R : C.signed_int;
   begin
      if C.pthread.pthread_attr_init (Attr'Access) = 0 then
         OK := C.pthread.pthread_getattr_np (Thread, Attr'Access) = 0
            and then C.pthread.pthread_attr_getstack (
               Attr'Access,
               C_Addr'Access,
               C_Size'Access) = 0;
         R := C.pthread.pthread_attr_destroy (Attr'Access);
         pragma Debug (Runtime_Error (R < 0,
            "failed to pthread_attr_destroy"));
      end if;
      if not OK then
         Top := Null_Address;
         Bottom := Null_Address;
      else
         Top := Cast (C_Addr);
         Bottom := Top + Storage_Elements.Storage_Offset (C_Size);
      end if;
   end Get;

end System.Native_Stack;
