pragma License (Unrestricted);
--  implementation unit
package System.Storage_Pools.Standard_Pools is
   pragma Preelaborate;

   type Standard_Pool is
      limited new Storage_Pools.Root_Storage_Pool with null record;
   pragma Finalize_Storage_Only (Standard_Pool);

   overriding procedure Allocate (
      Pool : in out Standard_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Standard_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Standard_Pool)
      return Storage_Elements.Storage_Count;

   --  the "standard storage pool" object defined in RM
   Standard_Storage_Pool : Standard_Pool := (
      Storage_Pools.Root_Storage_Pool with null record);

end System.Storage_Pools.Standard_Pools;
