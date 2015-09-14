pragma License (Unrestricted);
--  implementation unit for System.Initialization
private with Ada.Tags;
package System.Storage_Pools.Overlaps is
   pragma Preelaborate;

   type Overlay_Pool is limited new Root_Storage_Pool with
      null record; -- Actually, an allocation address is stored in TLS.
   pragma Finalize_Storage_Only (Overlay_Pool);

   procedure Set_Address (Storage_Address : Address);
   pragma Inline (Set_Address);

   overriding procedure Allocate (
      Pool : in out Overlay_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   pragma Inline (Allocate);

   overriding procedure Deallocate (
      Pool : in out Overlay_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   pragma Inline (Deallocate);

   overriding function Storage_Size (Pool : Overlay_Pool)
      return Storage_Elements.Storage_Count;
   pragma Inline (Storage_Size);

   Pool : constant not null access Overlay_Pool;

   --  Note: If it is declared as a local pool, Any objects allocated from it
   --    will be finalized when the local pool is out of scope,
   --    because the objects also belongs to the same scope.
   --  Therefore it should be declared in library-level.

private

   Dispatcher : aliased constant Ada.Tags.Tag := Overlay_Pool'Tag;

   Pool : constant not null access Overlay_Pool :=
      Overlay_Pool'Deref (Dispatcher'Address)'Unrestricted_Access;

end System.Storage_Pools.Overlaps;
