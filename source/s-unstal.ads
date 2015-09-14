pragma License (Unrestricted);
--  implementation unit
with System.Storage_Elements;
package System.Unbounded_Stack_Allocators is
   pragma Preelaborate;

   subtype Allocator_Type is Address;
      --  instead of Block_Access, for Runtime_Context and Secondary_Stack

   procedure Allocate (
      Allocator : aliased in out Allocator_Type;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   type Marker is private;

   function Mark (Allocator : aliased in out Allocator_Type)
      return Marker;

   procedure Release (
      Allocator : aliased in out Allocator_Type;
      Mark : Marker);

   procedure Clear (Allocator : aliased in out Allocator_Type);

private

   type Block;
   type Block_Access is access all Block;
   for Block_Access'Storage_Size use 0;
   type Block is record
      Previous : Address; -- Block_Access;
      Limit : Address; -- Last + 1
      Used : Address;
   end record;
   pragma Suppress_Initialization (Block);

   type Marker is record
      Top : Address; -- Block_Access;
      Used : Address;
   end record;
   pragma Suppress_Initialization (Marker);

end System.Unbounded_Stack_Allocators;
