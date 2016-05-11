pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with Ada.IO_Exceptions;
with System.Storage_Elements;
private package Ada.Numerics.Initiators is
   pragma Preelaborate;

   procedure Get (
      Item : System.Address;
      Size : System.Storage_Elements.Storage_Count);

   --  Exceptions

   Use_Error : exception
      renames IO_Exceptions.Use_Error;

end Ada.Numerics.Initiators;
