pragma License (Unrestricted);
--  implementation unit specialized for FreeBSD (or Linux)
with System.Storage_Elements;
package System.Unbounded_Allocators is
   --  Separated storage pool for local scope.
   pragma Preelaborate;

   type Unbounded_Allocator is limited private;

   procedure Initialize (Object : in out Unbounded_Allocator) is null;
   procedure Finalize (Object : in out Unbounded_Allocator);

   procedure Allocate (
      Allocator : in out Unbounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Deallocate (
      Allocator : in out Unbounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   function Storage_Size (Allocator : Unbounded_Allocator)
      return Storage_Elements.Storage_Count is
      (Storage_Elements.Storage_Count'Last);

private

   type Header;
   type Header_Access is access all Header;
   type Header is record
      Previous : Header_Access;
      Next : Header_Access;
   end record;
   pragma Suppress_Initialization (Header);

   type Unbounded_Allocator is new Header_Access;

end System.Unbounded_Allocators;
