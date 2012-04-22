with System.Soft_Links;
package body System.Storage_Pools.Overlaps is
   pragma Suppress (All_Checks);

   --  implementation

   procedure Set_Address (Storage_Address : Address) is
      TLS : constant not null Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
   begin
      TLS.Overlaid_Allocation := Storage_Address;
   end Set_Address;

   overriding procedure Allocate (
      Pool : in out Overlay_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
      TLS : constant not null Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
   begin
      Storage_Address := TLS.Overlaid_Allocation;
      TLS.Overlaid_Allocation := Null_Address;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Overlay_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
      pragma Unreferenced (Size_In_Storage_Elements);
      pragma Unreferenced (Alignment);
      TLS : constant not null Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
   begin
      pragma Assert (Storage_Address = TLS.Overlaid_Allocation);
      TLS.Overlaid_Allocation := Null_Address;
   end Deallocate;

   overriding function Storage_Size (Pool : Overlay_Pool)
      return Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return 0;
   end Storage_Size;

end System.Storage_Pools.Overlaps;
