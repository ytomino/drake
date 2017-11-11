pragma License (Unrestricted);
--  runtime unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with System.Storage_Elements;
package System.System_Allocators is
   pragma Preelaborate;

   --  heap

   Minimum_System_Allocator_Alignment : constant :=
      Standard'Address_Size / Standard'Storage_Unit;

   function Allocate (
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed
   function Allocate (
      Size : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
      return Address;

   procedure Free (Storage_Address : Address);

   function Reallocate (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed

   --  memory mapping

   function Page_Size return Storage_Elements.Storage_Count;

   function Map (
      Storage_Address : Address; -- not fixed but hint
      Size : Storage_Elements.Storage_Count)
      return Address; -- Null_Address if it failed
   procedure Unmap (
      Storage_Address : Address;
      Size : Storage_Elements.Storage_Count);

   --  Note: This package name comes from GNAT's implementation-defined
   --    attribute Standard'System_Allocator_Alignment.

end System.System_Allocators;
