package body System.Storage_Pools.Unbounded is

   overriding procedure Initialize (Object : in out Unbounded_Pool) is
   begin
      Unbounded_Allocators.Initialize (Object.Allocator);
   end Initialize;

   overriding procedure Finalize (Object : in out Unbounded_Pool) is
   begin
      Unbounded_Allocators.Finalize (Object.Allocator);
   end Finalize;

   overriding procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Unbounded_Allocators.Allocate (
         Pool.Allocator,
         Storage_Address => Storage_Address,
         Size_In_Storage_Elements => Size_In_Storage_Elements,
         Alignment => Alignment);
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Unbounded_Allocators.Deallocate (
         Pool.Allocator,
         Storage_Address => Storage_Address,
         Size_In_Storage_Elements => Size_In_Storage_Elements,
         Alignment => Alignment);
   end Deallocate;

end System.Storage_Pools.Unbounded;
