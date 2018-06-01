pragma License (Unrestricted);
--  extended unit specialized for Darwin
with System.Storage_Elements;
private with C.malloc.malloc;
package System.Storage_Pools.Unbounded is
   --  Separated storage pool for local scope.
   pragma Preelaborate;

   type Unbounded_Pool is limited new Root_Storage_Pool with private;

   procedure Initialize (Object : in out Unbounded_Pool);
   procedure Finalize (Object : in out Unbounded_Pool);

   procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   function Storage_Size (Pool : Unbounded_Pool)
      return Storage_Elements.Storage_Count is
      (Storage_Elements.Storage_Count'Last);

private

   type Unbounded_Pool is limited new Root_Storage_Pool with record
      Zone : C.malloc.malloc.malloc_zone_t_ptr;
   end record;
   pragma Finalize_Storage_Only (Unbounded_Pool);

end System.Storage_Pools.Unbounded;
