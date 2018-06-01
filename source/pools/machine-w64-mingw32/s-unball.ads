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

   --  Note: The custom value of Alignment is not supported in Windows.

private

   type Unbounded_Allocator is new C.winnt.HANDLE;

end System.Unbounded_Allocators;
