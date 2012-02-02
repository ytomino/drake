with System.Memory;
package body System.Pool_Global is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   overriding procedure Allocate (
      Pool : in out Unbounded_No_Reclaim_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
   begin
      Storage_Address := Memory.Allocate (Size_In_Storage_Elements);
      if Storage_Address mod Alignment /= 0 then
         Memory.Free (Storage_Address);
         raise Storage_Error;
      end if;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Unbounded_No_Reclaim_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
   begin
      Memory.Free (Storage_Address);
   end Deallocate;

   overriding function Storage_Size (Pool : Unbounded_No_Reclaim_Pool)
      return Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return Storage_Elements.Storage_Count'Last;
   end Storage_Size;

end System.Pool_Global;
