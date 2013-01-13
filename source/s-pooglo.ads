pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Elements;
with System.Storage_Pools;
package System.Pool_Global is
   pragma Preelaborate;

   type Unbounded_No_Reclaim_Pool is
      new Storage_Pools.Root_Storage_Pool with null record;
   pragma Finalize_Storage_Only (Unbounded_No_Reclaim_Pool);

   overriding procedure Allocate (
      Pool : in out Unbounded_No_Reclaim_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Unbounded_No_Reclaim_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Unbounded_No_Reclaim_Pool)
      return Storage_Elements.Storage_Count;

   --  required for default of 'Storage_Pool by compiler (s-pooglo.ads)
   Global_Pool_Object : Unbounded_No_Reclaim_Pool := (
      Storage_Pools.Root_Storage_Pool with null record);

end System.Pool_Global;
