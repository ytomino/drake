with System.Runtime_Context;
package body System.Storage_Pools.Overlaps is
   pragma Suppress (All_Checks);

   --  implementation

   procedure Set_Address (Storage_Address : Address) is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
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
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
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
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      pragma Assert (Storage_Address = TLS.Overlaid_Allocation);
      TLS.Overlaid_Allocation := Null_Address;
   end Deallocate;

end System.Storage_Pools.Overlaps;
