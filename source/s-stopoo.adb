package body System.Storage_Pools is
   pragma Suppress (All_Checks);

   procedure Allocate_Any (
      Pool : in out Root_Storage_Pool'Class;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Allocate (Pool, Storage_Address, Size_In_Storage_Elements, Alignment);
   end Allocate_Any;

   procedure Deallocate_Any (
      Pool : in out Root_Storage_Pool'Class;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Deallocate (Pool, Storage_Address, Size_In_Storage_Elements, Alignment);
   end Deallocate_Any;

end System.Storage_Pools;
