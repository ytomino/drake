pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.ws2tcpip;
package System.Native_IO.Sockets is
   pragma Preelaborate;
   pragma Linker_Options ("-lws2_32");

   type Port_Number is range 0 .. 16#ffff#;

   subtype End_Point is C.ws2tcpip.struct_addrinfoW_ptr;

   function Resolve (Host_Name : String; Service : String)
      return End_Point;
   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point;

   procedure Connect (Handle : out Handle_Type; Peer : End_Point);

   procedure Finalize (Item : End_Point);

end System.Native_IO.Sockets;
