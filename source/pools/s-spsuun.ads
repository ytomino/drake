pragma License (Unrestricted);
--  extended unit
private with System.Unbounded_Allocators;
package System.Storage_Pools.Subpools.Unbounded is
   --  The subpools version of System.Storage_Pools.Unbounded.
   pragma Preelaborate;

   type Unbounded_Pool_With_Subpools is
      limited new Root_Storage_Pool_With_Subpools with private;

   --  Note: Default_Subpool_For_Pool is not supported.

private

   type Unbounded_Subpool is limited new Root_Subpool with record
      Allocator : Unbounded_Allocators.Unbounded_Allocator;
   end record;

   type Unbounded_Pool_With_Subpools is
      limited new Root_Storage_Pool_With_Subpools with null record;
   pragma Finalize_Storage_Only (Unbounded_Pool_With_Subpools);

   overriding function Create_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools)
      return not null Subpool_Handle;

   overriding procedure Allocate_From_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle);

   overriding procedure Deallocate_Subpool (
      Pool : in out Unbounded_Pool_With_Subpools;
      Subpool : in out Subpool_Handle);

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool_With_Subpools;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

end System.Storage_Pools.Subpools.Unbounded;
