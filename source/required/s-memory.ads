pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Standard_Allocators;
package System.Memory is
   pragma Preelaborate;

   --  required for accessibility check, or task objects (s-memory.ads)
   procedure Free (Storage_Address : Address)
      renames Standard_Allocators.Free;

end System.Memory;
