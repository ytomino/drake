pragma License (Unrestricted);
--  implementation unit
with Ada.Tags; -- [gcc-5] is confused in C390004 if this line is "private with"
package System.Storage_Pools.Standard_Pools is
   pragma Preelaborate;

   type Standard_Pool is
      limited new Storage_Pools.Root_Storage_Pool with null record
      with Disable_Controlled => True;
   pragma Finalize_Storage_Only (Standard_Pool);

   overriding procedure Allocate (
      Pool : in out Standard_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding procedure Deallocate (
      Pool : in out Standard_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding function Storage_Size (Pool : Standard_Pool)
      return Storage_Elements.Storage_Count is
      (Storage_Elements.Storage_Count'Last);

   --  The "standard storage pool" object, is implementation-defined,
   --    but mentioned in RM 13.11(17).
   Standard_Storage_Pool : constant not null access Standard_Pool;

private

   Dispatcher : aliased constant Ada.Tags.Tag := Standard_Pool'Tag;

   Standard_Storage_Pool : constant not null access Standard_Pool :=
      Standard_Pool'Deref (Dispatcher'Address)'Unrestricted_Access;

end System.Storage_Pools.Standard_Pools;
