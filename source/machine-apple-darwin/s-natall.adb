pragma Check_Policy (Trace, Off);
with Ada;
with C.stdlib;
with C.sys.mman;
with C.unistd;
package body System.Native_Allocators is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   function Runtime_Error (
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity)
      return Boolean;
   pragma Import (Ada, Runtime_Error, "__drake_runtime_error");
   pragma Machine_Attribute (Runtime_Error, "noreturn");

   --  implementation

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Address (
         C.stdlib.malloc (
            C.size_t (Storage_Elements.Storage_Count'Max (1, Size))));
   end Allocate;

   procedure Free (Storage_Address : Address) is
   begin
      C.stdlib.free (C.void_ptr (Storage_Address));
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Address (
         C.stdlib.realloc (
            C.void_ptr (Storage_Address),
            C.size_t (Storage_Elements.Storage_Count'Max (1, Size))));
   end Reallocate;

   function Page_Size return Storage_Elements.Storage_Count is
   begin
      return Storage_Elements.Storage_Count (C.unistd.getpagesize);
   end Page_Size;

   function Map (
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      Mapped_Address : C.void_ptr;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Mapped_Address := C.sys.mman.mmap (
         C.void_ptr (Null_Address),
         C.size_t (Size),
         C.sys.mman.PROT_READ + C.sys.mman.PROT_WRITE,
         C.sys.mman.MAP_ANON + C.sys.mman.MAP_PRIVATE,
         -1,
         0);
      if Address (Mapped_Address) = Address (C.sys.mman.MAP_FAILED) then
         Mapped_Address := C.void_ptr (Null_Address); -- failed
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return Address (Mapped_Address);
   end Map;

   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      Mapped_Address : C.void_ptr;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Mapped_Address := C.sys.mman.mmap (
         C.void_ptr (Storage_Address),
         C.size_t (Size),
         C.sys.mman.PROT_READ + C.sys.mman.PROT_WRITE,
         C.sys.mman.MAP_ANON + C.sys.mman.MAP_PRIVATE + C.sys.mman.MAP_FIXED,
         -1,
         0);
      if Address (Mapped_Address) = Address (C.sys.mman.MAP_FAILED) then
         Mapped_Address := C.void_ptr (Null_Address); -- failed
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return Address (Mapped_Address);
   end Map;

   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count) is
      R : C.signed_int;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      R := C.sys.mman.munmap (C.void_ptr (Storage_Address), C.size_t (Size));
      pragma Check (Debug,
         Check => not (R < 0) or else Runtime_Error ("failed to unmap"));
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Unmap;

end System.Native_Allocators;
