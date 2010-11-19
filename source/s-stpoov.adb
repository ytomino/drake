package body System.Storage_Pools.Overlay is
   pragma Suppress (All_Checks);

   overriding procedure Allocate (
      Pool : in out Overlay_Storage_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
   begin
      Storage_Address := Pool.Overlay_Address;
   end Allocate;

   overriding function Storage_Size (Pool : Overlay_Storage_Pool)
      return Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return 0;
   end Storage_Size;

end System.Storage_Pools.Overlay;
