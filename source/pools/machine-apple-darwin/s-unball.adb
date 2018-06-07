with System.System_Allocators;
package body System.Unbounded_Allocators is

   procedure Initialize (Object : in out Unbounded_Allocator) is
   begin
      Object :=
         Unbounded_Allocator (C.malloc.malloc.malloc_create_zone (0, 0));
   end Initialize;

   procedure Finalize (Object : in out Unbounded_Allocator) is
   begin
      C.malloc.malloc.malloc_destroy_zone (
         C.malloc.malloc.malloc_zone_t_ptr (Object));
   end Finalize;

   procedure Allocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Storage_Address := Address (
         C.malloc.malloc.malloc_zone_memalign (
            C.malloc.malloc.malloc_zone_t_ptr (Allocator),
            C.size_t (
               Storage_Elements.Storage_Count'Max (
                  System_Allocators.Minimum_System_Allocator_Alignment,
                  Alignment)),
            C.size_t (Size_In_Storage_Elements)));
      if Storage_Address = Null_Address then
         raise Storage_Error;
      end if;
   end Allocate;

   procedure Deallocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
   begin
      C.malloc.malloc.malloc_zone_free (
         C.malloc.malloc.malloc_zone_t_ptr (Allocator),
         C.void_ptr (Storage_Address));
   end Deallocate;

   function Allocator_Of (Storage_Address : Address)
      return Unbounded_Allocator is
   begin
      return Unbounded_Allocator (
         C.malloc.malloc.malloc_zone_from_ptr (
            C.void_const_ptr (Storage_Address)));
   end Allocator_Of;

end System.Unbounded_Allocators;
