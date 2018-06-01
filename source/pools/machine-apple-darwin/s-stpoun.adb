with System.System_Allocators;
package body System.Storage_Pools.Unbounded is

   procedure Initialize (Object : in out Unbounded_Pool) is
   begin
      Object.Zone := C.malloc.malloc.malloc_create_zone (0, 0);
   end Initialize;

   procedure Finalize (Object : in out Unbounded_Pool) is
   begin
      C.malloc.malloc.malloc_destroy_zone (Object.Zone);
   end Finalize;

   procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Storage_Address := Address (
         C.malloc.malloc.malloc_zone_memalign (
            Pool.Zone,
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
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
   begin
      C.malloc.malloc.malloc_zone_free (
         Pool.Zone,
         C.void_ptr (Storage_Address));
   end Deallocate;

end System.Storage_Pools.Unbounded;
