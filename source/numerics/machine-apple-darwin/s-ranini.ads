pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.IO_Exceptions;
with System.Storage_Elements;
package System.Random_Initiators is
   pragma Preelaborate;

   procedure Get (
      Item : Address;
      Size : Storage_Elements.Storage_Count);

   --  Exceptions

   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

end System.Random_Initiators;
