pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with C.netdb;
package Ada.Streams.Stream_IO.Sockets is
   --  There is a function to create socket.
   pragma Preelaborate;

   type Port_Number is range 0 .. 16#ffff#;

   type Address_Information is limited private;

   function Get (Host_Name : String; Service : String)
      return Address_Information;
   function Get (Host_Name : String; Port : Port_Number)
      return Address_Information;

   procedure Connect (File : in out File_Type; Host : Address_Information);
   function Connect (Host : Address_Information) return File_Type;

private

   type Address_Information is new Finalization.Limited_Controlled with record
      Data : C.netdb.struct_addrinfo_ptr := null;
   end record;

   overriding procedure Finalize (Object : in out Address_Information);

end Ada.Streams.Stream_IO.Sockets;
