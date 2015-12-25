with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with System.Formatting;
with System.Once;
with System.Termination;
with System.Zero_Terminated_WStrings;
with C.psdk_inc.qwsadata;
with C.winnt;
with C.winsock2;
package body System.Native_IO.Sockets is
   use Ada.Exception_Identification.From_Here;
   use type C.signed_int;
   use type C.size_t;
   use type C.psdk_inc.qsocket_types.SOCKET;
   use type C.ws2tcpip.struct_addrinfoW_ptr;

   Flag : aliased Once.Flag := 0;
   Failed_To_Initialize : Boolean;
   Data : aliased C.psdk_inc.qwsadata.WSADATA := (others => <>);

   procedure Finalize;
   procedure Finalize is
   begin
      if C.winsock2.WSACleanup /= 0 then
         null; -- ignore error
      end if;
   end Finalize;

   procedure Initialize;
   procedure Initialize is
   begin
      Termination.Register_Exit (Finalize'Access);
      if C.winsock2.WSAStartup (16#0202#, Data'Access) /= 0 then
         Failed_To_Initialize := True;
      end if;
   end Initialize;

   procedure Check_Initialize;
   procedure Check_Initialize is
   begin
      Once.Initialize (Flag'Access, Initialize'Access);
      if Failed_To_Initialize then
         raise Program_Error; -- ??
      end if;
   end Check_Initialize;

   --  client

   function Get (
      Host_Name : not null access constant C.winnt.WCHAR;
      Service : not null access constant C.winnt.WCHAR;
      Hints : not null access constant C.ws2tcpip.struct_addrinfoW)
      return End_Point;
   function Get (
      Host_Name : not null access constant C.winnt.WCHAR;
      Service : not null access constant C.winnt.WCHAR;
      Hints : not null access constant C.ws2tcpip.struct_addrinfoW)
      return End_Point
   is
      Data : aliased C.ws2tcpip.struct_addrinfoW_ptr;
      R : C.signed_int;
   begin
      R := C.ws2tcpip.GetAddrInfoW (Host_Name, Service, Hints, Data'Access);
      if R /= 0 then
         return null; -- Use_Error
      else
         return Data;
      end if;
   end Get;

   --  implementation of client

   function Resolve (Host_Name : String; Service : String)
      return End_Point is
   begin
      Check_Initialize;
      declare
         Hints : aliased constant C.ws2tcpip.struct_addrinfoW := (
            ai_flags => 0,
            ai_family => C.winsock2.AF_UNSPEC,
            ai_socktype => C.winsock2.SOCK_STREAM,
            ai_protocol => C.winsock2.IPPROTO_TCP,
            ai_addrlen => 0,
            ai_canonname => null,
            ai_addr => null,
            ai_next => null);
         W_Host_Name : C.winnt.WCHAR_array (
            0 ..
            Host_Name'Length * Zero_Terminated_WStrings.Expanding);
         W_Service : C.winnt.WCHAR_array (
            0 ..
            Service'Length * Zero_Terminated_WStrings.Expanding);
      begin
         Zero_Terminated_WStrings.To_C (
            Host_Name,
            W_Host_Name (0)'Access);
         Zero_Terminated_WStrings.To_C (
            Service,
            W_Service (0)'Access);
         return Get (
            W_Host_Name (0)'Access,
            W_Service (0)'Access,
            Hints'Access);
      end;
   end Resolve;

   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point is
   begin
      Check_Initialize;
      declare
         Hints : aliased constant C.ws2tcpip.struct_addrinfoW := (
            ai_flags => 0, -- mingw-w64 header does not have AI_NUMERICSERV
            ai_family => C.winsock2.AF_UNSPEC,
            ai_socktype => C.winsock2.SOCK_STREAM,
            ai_protocol => C.winsock2.IPPROTO_TCP,
            ai_addrlen => 0,
            ai_canonname => null,
            ai_addr => null,
            ai_next => null);
         W_Host_Name : C.winnt.WCHAR_array (
            0 ..
            Host_Name'Length * Zero_Terminated_WStrings.Expanding);
         Service : String (1 .. 5);
         Service_Last : Natural;
         W_Service : C.winnt.WCHAR_array (
            0 ..
            Service'Length * Zero_Terminated_WStrings.Expanding);
         Error : Boolean;
      begin
         Zero_Terminated_WStrings.To_C (
            Host_Name,
            W_Host_Name (0)'Access);
         Formatting.Image (
            Formatting.Unsigned (Port),
            Service,
            Service_Last,
            Base => 10,
            Error => Error);
         Zero_Terminated_WStrings.To_C (
            Service (1 .. Service_Last),
            W_Service (0)'Access);
         return Get (
            W_Host_Name (0)'Access,
            W_Service (0)'Access,
            Hints'Access);
      end;
   end Resolve;

   procedure Connect (Handle : aliased out Handle_Type; Peer : End_Point) is
      function Cast is
         new Ada.Unchecked_Conversion (
            C.psdk_inc.qsocket_types.SOCKET,
            C.winnt.HANDLE);
      Socket : C.psdk_inc.qsocket_types.SOCKET;
      I : C.ws2tcpip.struct_addrinfoW_ptr := Peer;
   begin
      while I /= null loop
         Socket := C.winsock2.WSASocket (
            I.ai_family,
            I.ai_socktype,
            I.ai_protocol,
            null,
            0,
            0);
         if Socket /= C.psdk_inc.qsocket_types.INVALID_SOCKET then
            if C.winsock2.WSAConnect (
               Socket,
               I.ai_addr,
               C.signed_int (I.ai_addrlen),
               null,
               null,
               null,
               null) = 0
            then
               --  connected
               Handle := Cast (Socket);
               return;
            end if;
            if C.winsock2.closesocket (Socket) /= 0 then
               exit; -- Use_Error or Device_Error ?
            end if;
         end if;
         I := I.ai_next;
      end loop;
      Handle := Invalid_Handle;
   end Connect;

   procedure Finalize (Item : End_Point) is
   begin
      C.ws2tcpip.FreeAddrInfoW (Item);
   end Finalize;

   --  implementation of server

   procedure Listen (Server : aliased out Listener; Port : Port_Number) is
      function To_char_const_ptr is
         new Ada.Unchecked_Conversion (
            C.signed_int_ptr, -- pointer to BOOL
            C.char_const_ptr);
      Hints : aliased constant C.ws2tcpip.struct_addrinfoW := (
         ai_flags => C.ws2tcpip.AI_PASSIVE, -- or AI_NUMERICSERV
         ai_family => C.winsock2.AF_UNSPEC,
         ai_socktype => C.winsock2.SOCK_STREAM,
         ai_protocol => C.winsock2.IPPROTO_TCP,
         ai_addrlen => 0,
         ai_canonname => null,
         ai_addr => null,
         ai_next => null);
      Data : aliased C.ws2tcpip.struct_addrinfoW_ptr;
      W_Service : C.winnt.WCHAR_array (0 .. 5); -- "65535" & NUL
      Reuse_Addr_Option : aliased C.windef.BOOL;
   begin
      Check_Initialize;
      declare
         Service : String (1 .. 5);
         Service_Last : Natural;
         Error : Boolean;
      begin
         Formatting.Image (
            Formatting.Unsigned (Port),
            Service,
            Service_Last,
            Base => 10,
            Error => Error);
         Zero_Terminated_WStrings.To_C (
            Service (1 .. Service_Last),
            W_Service (0)'Access);
      end;
      if C.ws2tcpip.GetAddrInfoW (
         null,
         W_Service (0)'Access,
         Hints'Access,
         Data'Access) /= 0
      then
         Server := Invalid_Listener; -- Use_Error
      else
         Server := C.winsock2.WSASocket (
            Data.ai_family,
            Data.ai_socktype,
            Data.ai_protocol,
            null,
            0,
            0);
         --  set SO_REUSEADDR
         Reuse_Addr_Option := 1;
         if C.winsock2.setsockopt (
            Server,
            C.winsock2.SOL_SOCKET,
            C.winsock2.SO_REUSEADDR,
            To_char_const_ptr (Reuse_Addr_Option'Unchecked_Access),
            Reuse_Addr_Option'Size / Standard'Storage_Unit) /= 0
         then
            Close_Listener (Server, Raise_On_Error => False);
            C.ws2tcpip.FreeAddrInfoW (Data);
            Server := Invalid_Listener; -- Use_Error
         end if;
         --  bind
         if C.winsock2.bind (
            Server,
            Data.ai_addr,
            C.signed_int (Data.ai_addrlen)) /= 0
         then
            Close_Listener (Server, Raise_On_Error => False);
            C.ws2tcpip.FreeAddrInfoW (Data);
            Server := Invalid_Listener; -- Use_Error
         end if;
         --  free
         C.ws2tcpip.FreeAddrInfoW (Data);
         --  listen
         if C.winsock2.listen (Server, C.winsock2.SOMAXCONN) /= 0 then
            Close_Listener (Server, Raise_On_Error => False);
            Server := Invalid_Listener; -- Use_Error
         end if;
      end if;
   end Listen;

   procedure Accept_Socket (
      Server : Listener;
      Handle : aliased out Handle_Type;
      Remote_Address : out Socket_Address)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            C.psdk_inc.qsocket_types.SOCKET,
            C.winnt.HANDLE);
      Len : aliased C.windef.INT :=
         Socket_Address'Size / Standard'Storage_Unit;
      R : C.psdk_inc.qsocket_types.SOCKET;
   begin
      R := C.winsock2.WSAAccept (
         Server,
         Remote_Address'Unrestricted_Access,
         Len'Access,
         lpfnCondition => null,
         dwCallbackData => 0);
      if R = C.psdk_inc.qsocket_types.INVALID_SOCKET then
         Handle := Invalid_Handle; -- Use_Error
      else
         Handle := Cast (R);
      end if;
   end Accept_Socket;

   procedure Close_Listener (Server : Listener; Raise_On_Error : Boolean) is
      R : C.signed_int;
   begin
      R := C.winsock2.closesocket (Server);
      if R /= 0 and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Listener;

end System.Native_IO.Sockets;
