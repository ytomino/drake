pragma License (Unrestricted);
--  implementation unit specialized for Linux
with C.netdb;
with C.sys.socket;
package System.Native_IO.Sockets is
   pragma Preelaborate;

   type Port_Number is range 0 .. 16#ffff#;

   subtype Socket_Address is C.sys.socket.struct_sockaddr;

   --  client

   subtype End_Point is C.netdb.struct_addrinfo_ptr;

   function Resolve (Host_Name : String; Service : String)
      return End_Point;
   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point;

   procedure Connect (Handle : aliased out Handle_Type; Peer : End_Point);

   procedure Finalize (Item : End_Point);

   --  server

   subtype Listener is Handle_Type;

   Invalid_Listener : Listener
      renames Invalid_Handle;

   procedure Listen (Server : aliased out Listener; Port : Port_Number);

   procedure Accept_Socket (
      Server : Listener;
      Handle : aliased out Handle_Type;
      Remote_Address : out Socket_Address);

   procedure Close_Listener (Server : Listener; Raise_On_Error : Boolean);

end System.Native_IO.Sockets;
