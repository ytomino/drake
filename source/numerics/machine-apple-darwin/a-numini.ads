pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
with System.Storage_Elements;
private procedure Ada.Numerics.Initiator (
   Item : System.Address;
   Size : System.Storage_Elements.Storage_Count);
pragma Preelaborate (Ada.Numerics.Initiator);
