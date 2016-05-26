pragma License (Unrestricted);
--  implementation unit specialized for Windows
with C.psdk_inc.qip_types;
with C.psdk_inc.qsocket_types;
with C.ws2tcpip;
package System.Native_IO.Sockets is
   pragma Preelaborate;
   pragma Linker_Options ("-lws2_32");

   type Port_Number is range 0 .. 16#ffff#;

   subtype Socket_Address is C.psdk_inc.qip_types.SOCKADDR;

   procedure Close_Socket (Handle : Handle_Type; Raise_On_Error : Boolean);
      --  Close_Ordinary without Name

   --  client

   subtype End_Point is C.ws2tcpip.struct_addrinfoW_ptr;

   function Resolve (Host_Name : String; Service : String)
      return End_Point;
   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point;

   procedure Connect (Handle : aliased out Handle_Type; Peer : End_Point);

   procedure Finalize (Item : End_Point);

   --  server

   subtype Listener is C.psdk_inc.qsocket_types.SOCKET;

   Invalid_Listener : constant Listener :=
      C.psdk_inc.qsocket_types.INVALID_SOCKET;

   procedure Listen (Server : aliased out Listener; Port : Port_Number);

   procedure Accept_Socket (
      Server : Listener;
      Handle : aliased out Handle_Type;
      Remote_Address : out Socket_Address);

   procedure Close_Listener (Server : Listener; Raise_On_Error : Boolean);

end System.Native_IO.Sockets;
