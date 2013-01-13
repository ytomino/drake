pragma License (Unrestricted);
--  extended unit
private with C.malloc.malloc;
package System.Storage_Pools.Zones is
   --  This package provides a separated stogae pool for local scope.
   pragma Preelaborate;

   type Zone_Pool is new Root_Storage_Pool with private;

private

   type Zone_Pool is new Root_Storage_Pool with record
      Zone : C.malloc.malloc.malloc_zone_t_ptr;
   end record;

   overriding procedure Initialize (Object : in out Zone_Pool);
   overriding procedure Finalize (Object : in out Zone_Pool);

   overriding procedure Allocate (
      Pool : in out Zone_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Zone_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Zone_Pool)
      return Storage_Elements.Storage_Count;

end System.Storage_Pools.Zones;
