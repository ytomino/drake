pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Unwind.Raising; -- raising exception in compiler unit
with System.Unwind.Standard;
with C.stdlib;
with C.sys.mman;
with C.unistd;
package body System.Standard_Allocators is
   pragma Suppress (All_Checks);
   use type C.signed_int;

   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Ada.Debug.Source_Location;
      Enclosing_Entity : String := Ada.Debug.Enclosing_Entity);
   pragma Import (Ada, Runtime_Error, "__drake_runtime_error");

   Heap_Exhausted : constant String := "heap exhausted";

   --  implementation

   function Allocate (Size : Storage_Elements.Storage_Count)
      return Address
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      return Result : constant Address := Cast (C.stdlib.malloc (
         C.size_t (Storage_Elements.Storage_Count'Max (1, Size))))
      do
         if Result = Null_Address then
            Unwind.Raising.Raise_Exception_From_Here_With (
               Unwind.Standard.Storage_Error'Access,
               Message => Heap_Exhausted);
         end if;
      end return;
   end Allocate;

   procedure Free (Storage_Address : Address) is
   begin
      C.stdlib.free (C.void_ptr (Storage_Address));
   end Free;

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
   begin
      return Result : constant Address := Cast (C.stdlib.realloc (
         C.void_ptr (Storage_Address),
         C.size_t (Storage_Elements.Storage_Count'Max (1, Size))))
      do
         if Result = Null_Address then
            Unwind.Raising.Raise_Exception_From_Here_With (
               Unwind.Standard.Storage_Error'Access,
               Message => Heap_Exhausted);
         end if;
      end return;
   end Reallocate;

   Page_Exhausted : constant String := "page exhausted";

   function Page_Size return Storage_Elements.Storage_Count is
   begin
      return Storage_Elements.Storage_Count (C.unistd.getpagesize);
   end Page_Size;

   function Map (
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
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
      if Cast (Mapped_Address) = Cast (C.sys.mman.MAP_FAILED)
         and then Raise_On_Error
      then
         Unwind.Raising.Raise_Exception_From_Here_With (
            Unwind.Standard.Storage_Error'Access,
            Message => Page_Exhausted);
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return Cast (Mapped_Address);
   end Map;

   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address
   is
      function Cast is new Ada.Unchecked_Conversion (C.void_ptr, Address);
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
      if Cast (Mapped_Address) = Cast (C.sys.mman.MAP_FAILED)
         and then Raise_On_Error
      then
         Unwind.Raising.Raise_Exception_From_Here_With (
            Unwind.Standard.Storage_Error'Access,
            Message => Page_Exhausted);
      end if;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return Cast (Mapped_Address);
   end Map;

   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count) is
      R : C.signed_int;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      R := C.sys.mman.munmap (C.void_ptr (Storage_Address), C.size_t (Size));
      pragma Debug (Runtime_Error (R < 0, "failed to unmap"));
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Unmap;

end System.Standard_Allocators;
