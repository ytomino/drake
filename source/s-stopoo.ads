pragma License (Unrestricted);
with Ada.Finalization;
with System.Storage_Elements;
package System.Storage_Pools is
   pragma Preelaborate;

   type Root_Storage_Pool is
      abstract limited new Ada.Finalization.Limited_Controlled with private;
   pragma Preelaborable_Initialization (Root_Storage_Pool);

   procedure Allocate (
      Pool : in out Root_Storage_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is abstract;

   procedure Deallocate (
      Pool : in out Root_Storage_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is abstract;

   function Storage_Size (Pool : Root_Storage_Pool)
      return Storage_Elements.Storage_Count is abstract;

   --  to use in System.Finalization_Masters
   type Storage_Pool_Access is access all Root_Storage_Pool'Class;
   for Storage_Pool_Access'Storage_Size use 0;

private

   type Root_Storage_Pool is
      abstract limited new Ada.Finalization.Limited_Controlled with
      null record;

   --  required for allocation with explicit 'Storage_Pool by compiler
   --  (s-stopoo.ads)
   procedure Allocate_Any (
      Pool : in out Root_Storage_Pool'Class;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   --  required for deallocation with explicit 'Storage_Pool by compiler
   --  (s-stopoo.ads)
   procedure Deallocate_Any (
      Pool : in out Root_Storage_Pool'Class;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   --  required for extra parameter of built-in-place (s-stopoo.ads)
   subtype Root_Storage_Pool_Ptr is Storage_Pool_Access;

end System.Storage_Pools;
