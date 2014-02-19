pragma License (Unrestricted);
--  runtime unit
with System.Storage_Elements;
package System.Standard_Allocators is
   pragma Preelaborate;

   --  heap (s-memory.ads)

   function Allocate (Size : Storage_Elements.Storage_Count) return Address;
   pragma Export (C, Allocate, "__gnat_malloc");

   procedure Free (Storage_Address : Address);
   pragma Export (C, Free, "__gnat_free");

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address;
   pragma Export (C, Reallocate, "__gnat_realloc");

   --  memory mapping

   function Page_Size return Storage_Elements.Storage_Count;

   function Map (
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address;
   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count;
      Raise_On_Error : Boolean := True)
      return Address;
   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count);

end System.Standard_Allocators;
