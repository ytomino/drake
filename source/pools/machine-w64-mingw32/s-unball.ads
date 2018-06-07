pragma License (Unrestricted);
--  implementation unit specialized for Windows
with System.Storage_Elements;
private with C.winnt;
package System.Unbounded_Allocators is
   --  Separated storage pool for local scope.
   pragma Preelaborate;

   type Unbounded_Allocator is limited private;

   procedure Initialize (Object : in out Unbounded_Allocator);
   procedure Finalize (Object : in out Unbounded_Allocator);

   procedure Allocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Deallocate (
      Allocator : Unbounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   function Allocator_Of (Storage_Address : Address)
      return Unbounded_Allocator;

   --  Note: The custom value of Alignment is not supported in Windows.

private

   type Unbounded_Allocator is new C.winnt.HANDLE;

end System.Unbounded_Allocators;
