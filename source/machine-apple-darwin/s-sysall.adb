pragma Check_Policy (Trace => Ignore);
with Ada; -- assertions
with System.Debug; -- assertions
with C.stdlib;
with C.sys.mman;
with C.unistd;
package body System.System_Allocators is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   --  implementation

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      return Address (
         C.stdlib.malloc (
            C.size_t (Storage_Elements.Storage_Count'Max (1, Size))));
   end Allocate;

   function Allocate (
      Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
      return Address
   is
      Result : aliased C.void_ptr;
   begin
      if C.stdlib.posix_memalign (
         Result'Access,
         C.size_t (
            Storage_Elements.Storage_Count'Max (
               Minimum_System_Allocator_Alignment,
               Alignment)),
         C.size_t (Storage_Elements.Storage_Count'Max (1, Size))) /= 0
      then
         return Null_Address;
      else
         return Address (Result);
      end if;
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
      return Storage_Elements.Storage_Offset (C.unistd.getpagesize);
   end Page_Size;

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
         C.sys.mman.MAP_ANON + C.sys.mman.MAP_PRIVATE,
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
      Size : Storage_Elements.Storage_Count)
   is
      R : C.signed_int;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      R := C.sys.mman.munmap (C.void_ptr (Storage_Address), C.size_t (Size));
      pragma Check (Debug,
         Check => not (R < 0) or else Debug.Runtime_Error ("munmap failed"));
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Unmap;

end System.System_Allocators;
