pragma License (Unrestricted);
--  extended unit
private with System.Unbounded_Allocators;
package System.Storage_Pools.Unbounded is
   --  Separated storage pool for local scope.
   --  The compiler (gcc) does not support scope-based automatic deallocation.
   --  Instead, some custom pool, like System.Pool_Local.Unbounded_Reclaim_Pool
   --    of GNAT runtime, using separated storage by every pool object and
   --    having different lifetime from the standard storage pool can be used
   --    for deallocating all of allocated from each pool object, at once.
   --  This package provides the similar pool.
   pragma Preelaborate;

   type Unbounded_Pool is limited new Root_Storage_Pool with private;
   pragma Unreferenced_Objects (Unbounded_Pool); -- [gcc-4.8] warnings

private

   type Unbounded_Pool is limited new Root_Storage_Pool with record
      Allocator : Unbounded_Allocators.Unbounded_Allocator;
   end record;
   pragma Finalize_Storage_Only (Unbounded_Pool);

   overriding procedure Initialize (Object : in out Unbounded_Pool);
   overriding procedure Finalize (Object : in out Unbounded_Pool);

   overriding procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Unbounded_Pool)
      return Storage_Elements.Storage_Count;

end System.Storage_Pools.Unbounded;
