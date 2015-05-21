pragma License (Unrestricted);
--  runtime unit specialized for Windows
with System.Storage_Elements;
package System.Native_Allocators is
   pragma Preelaborate;

   --  heap

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed

   procedure Free (Storage_Address : Address);

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed

   --  memory mapping

   function Page_Size return Storage_Elements.Storage_Count;

   function Map (
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed
   function Map (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed
   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count);

end System.Native_Allocators;
