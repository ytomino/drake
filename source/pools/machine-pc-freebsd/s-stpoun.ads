pragma License (Unrestricted);
--  extended unit
package System.Storage_Pools.Unbounded is
   --  This package provides a separated stogae pool for local scope.
   pragma Preelaborate;

   type Unbounded_Pool is limited new Root_Storage_Pool with private;

private

   type Header;
   type Header_Access is access all Header;
   type Header is record
      Previous : Header_Access;
      Next : Header_Access;
   end record;
   pragma Suppress_Initialization (Header);

   type Unbounded_Pool is limited new Root_Storage_Pool with record
      List : Header_Access := null;
   end record;
   pragma Finalize_Storage_Only (Unbounded_Pool);

   overriding procedure Finalize (Object : in out Unbounded_Pool);

   overriding procedure Allocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Unbounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Unbounded_Pool)
      return Storage_Elements.Storage_Count;

end System.Storage_Pools.Unbounded;
