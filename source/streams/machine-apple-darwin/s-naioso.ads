pragma License (Unrestricted);
--  implementation unit specialized for Darwin (or FreeBSD)
with C.netdb;
package System.Native_IO.Sockets is
   pragma Preelaborate;

   type Port_Number is range 0 .. 16#ffff#;

   subtype End_Point is C.netdb.struct_addrinfo_ptr;

   function Resolve (Host_Name : String; Service : String)
      return End_Point;
   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point;

   procedure Connect (Handle : aliased out Handle_Type; Peer : End_Point);

   procedure Finalize (Item : End_Point);

end System.Native_IO.Sockets;
