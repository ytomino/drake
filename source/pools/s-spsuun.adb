package body System.Storage_Pools.Subpools.Unbounded is

   type Unbounded_Subpool_Access is access all Unbounded_Subpool;

   --  implementation

   overriding function Create_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools)
      return not null Subpool_Handle
   is
      Subpool : constant not null Unbounded_Subpool_Access :=
         new Unbounded_Subpool;
   begin
      Unbounded_Allocators.Initialize (Subpool.Allocator);
      declare
         Result : constant not null Subpool_Handle :=
            Subpool_Handle (Subpool);
      begin
         Set_Pool_Of_Subpool (Result, Pool);
         return Result;
      end;
   end Create_Subpool;

   overriding procedure Allocate_From_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle)
   is
      pragma Unreferenced (Pool);
   begin
      Unbounded_Allocators.Allocate (
         Unbounded_Subpool_Access (Subpool).Allocator,
         Storage_Address => Storage_Address,
         Size_In_Storage_Elements => Size_In_Storage_Elements,
         Alignment => Alignment);
   end Allocate_From_Subpool;

   overriding procedure Deallocate_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools;
      Subpool : in out Subpool_Handle)
   is
      pragma Unreferenced (Pool);
   begin
      Unbounded_Allocators.Finalize (
         Unbounded_Subpool_Access (Subpool).Allocator);
   end Deallocate_Subpool;

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool_With_Subpools;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
   begin
      Unbounded_Allocators.Deallocate (
         Unbounded_Allocators.Allocator_Of (Storage_Address),
         Storage_Address => Storage_Address,
         Size_In_Storage_Elements => Size_In_Storage_Elements,
         Alignment => Alignment);
   end Deallocate;

end System.Storage_Pools.Subpools.Unbounded;
