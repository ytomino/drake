with System.Standard_Allocators;
with System.System_Allocators;
package body System.Storage_Pools.Standard_Pools is
   pragma Suppress (All_Checks);

   overriding procedure Allocate (
      Pool : in out Standard_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
   begin
      Storage_Address :=
         System_Allocators.Allocate (
            Size_In_Storage_Elements,
            Alignment => Alignment);
      if Storage_Address = Null_Address then
         Standard_Allocators.Raise_Heap_Exhausted;
      end if;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Standard_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
   begin
      System_Allocators.Free (Storage_Address);
   end Deallocate;

end System.Storage_Pools.Standard_Pools;
