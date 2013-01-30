pragma License (Unrestricted);
--  runtime unit
with System.Storage_Elements;
package System.Memory is
   pragma Preelaborate;

   --  heap (s-memory.ads)

   function Allocate (Size : Storage_Elements.Storage_Count) return Address;
   pragma Export (C, Allocate, "__gnat_malloc");

   procedure Free (P : Address);
   pragma Export (C, Free, "__gnat_free");

   function Reallocate (P : Address; Size : Storage_Elements.Storage_Count)
      return Address;
   pragma Export (C, Reallocate, "__gnat_realloc");

   --  memory mapping

   function Page_Size return Storage_Elements.Storage_Count;

   function Map (
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address;
   function Map (
      P : Address;
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address;
   procedure Unmap (P : Address; Size : Storage_Elements.Storage_Count);

end System.Memory;
