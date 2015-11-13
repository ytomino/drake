with Ada.Exception_Identification.From_Here;
with System.Formatting;
with System.Synchronous_Control;
with C.errno;
with C.bits.socket;
with C.netinet.in_h;
with C.unistd;
package body System.Native_IO.Sockets is
   use Ada.Exception_Identification.From_Here;
   use type C.size_t;
   use type C.netdb.struct_addrinfo_ptr;
   use type C.unistd.socklen_t;

   --  client

   function Get (
      Host_Name : not null access constant C.char;
      Service : not null access constant C.char;
      Hints : not null access constant C.netdb.struct_addrinfo)
      return End_Point;
   function Get (
      Host_Name : not null access constant C.char;
      Service : not null access constant C.char;
      Hints : not null access constant C.netdb.struct_addrinfo)
      return End_Point
   is
      Data : aliased C.netdb.struct_addrinfo_ptr;
      R : C.signed_int;
   begin
      R := C.netdb.getaddrinfo (Host_Name, Service, Hints, Data'Access);
      if R /= 0 then
         return null; -- Use_Error
      else
         return Data;
      end if;
   end Get;

   --  implementation of client

   function Resolve (Host_Name : String; Service : String)
      return End_Point
   is
      Hints : aliased constant C.netdb.struct_addrinfo := (
         ai_flags => 0,
         ai_family => C.sys.socket.AF_UNSPEC,
         ai_socktype => C.sys.socket.enum_socket_type'Enum_Rep (
            C.sys.socket.SOCK_STREAM),
         ai_protocol => C.netinet.in_h.Cast (
            C.netinet.in_h.IPPROTO_TCP),
         ai_addrlen => 0,
         ai_canonname => null,
         ai_addr => null,
         ai_next => null);
      C_Host_Name : C.char_array (
         0 ..
         Host_Name'Length * Zero_Terminated_Strings.Expanding);
      C_Service : C.char_array (
         0 ..
         Service'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Host_Name, C_Host_Name (0)'Access);
      Zero_Terminated_Strings.To_C (Service, C_Service (0)'Access);
      return Get (C_Host_Name (0)'Access, C_Service (0)'Access, Hints'Access);
   end Resolve;

   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point
   is
      Hints : aliased constant C.netdb.struct_addrinfo := (
         ai_flags => C.netdb.AI_NUMERICSERV,
         ai_family => C.sys.socket.AF_UNSPEC,
         ai_socktype => C.sys.socket.enum_socket_type'Enum_Rep (
            C.sys.socket.SOCK_STREAM),
         ai_protocol => C.netinet.in_h.Cast (
            C.netinet.in_h.IPPROTO_TCP),
         ai_addrlen => 0,
         ai_canonname => null,
         ai_addr => null,
         ai_next => null);
      C_Host_Name : C.char_array (
         0 ..
         Host_Name'Length * Zero_Terminated_Strings.Expanding);
      Service : C.char_array (0 .. 5); -- "65535" & NUL
      Service_Length : C.size_t;
      Error : Boolean;
   begin
      Zero_Terminated_Strings.To_C (Host_Name, C_Host_Name (0)'Access);
      declare
         Service_As_String : String (1 .. 5);
         for Service_As_String'Address use Service'Address;
         Service_Last : Natural;
      begin
         Formatting.Image (
            Formatting.Unsigned (Port),
            Service_As_String,
            Service_Last,
            Base => 10,
            Error => Error);
         Service_Length := C.size_t (Service_Last);
      end;
      Service (Service_Length) := C.char'Val (0);
      return Get (C_Host_Name (0)'Access, Service (0)'Access, Hints'Access);
   end Resolve;

   procedure Connect (Handle : aliased out Handle_Type; Peer : End_Point) is
      I : C.netdb.struct_addrinfo_ptr := Peer;
   begin
      while I /= null loop
         Handle := C.sys.socket.socket (
            I.ai_family,
            I.ai_socktype,
            I.ai_protocol);
         if Handle >= 0 then
            if C.sys.socket.connect (
               Handle,
               C.sys.socket.CONST_SOCKADDR_ARG'(
                  Unchecked_Tag => 0,
                  sockaddr => C.bits.socket.struct_sockaddr_const_ptr (
                     I.ai_addr)),
               I.ai_addrlen) = 0
            then
               --  connected
               Set_Close_On_Exec (Handle);
               return;
            end if;
            if C.unistd.close (Handle) < 0 then
               exit; -- Use_Error or Device_Error ?
            end if;
         end if;
         I := I.ai_next;
      end loop;
      Handle := Invalid_Handle;
   end Connect;

   procedure Finalize (Item : End_Point) is
   begin
      C.netdb.freeaddrinfo (Item);
   end Finalize;

   --  implementation of server

   procedure Listen (Server : aliased out Listener; Port : Port_Number) is
      Hints : aliased constant C.netdb.struct_addrinfo := (
         ai_flags => C.signed_int (
            C.unsigned_int'(C.netdb.AI_PASSIVE or C.netdb.AI_NUMERICSERV)),
         ai_family => C.sys.socket.AF_UNSPEC,
         ai_socktype =>
            C.sys.socket.enum_socket_type'Enum_Rep (C.sys.socket.SOCK_STREAM),
         ai_protocol => C.netinet.in_h.Cast (C.netinet.in_h.IPPROTO_TCP),
         ai_addrlen => 0,
         ai_canonname => null,
         ai_addr => null,
         ai_next => null);
      Data : aliased C.netdb.struct_addrinfo_ptr;
      Service : C.char_array (0 .. 5); -- "65535" & NUL
      Service_Length : C.size_t;
      Reuse_Addr_Option : aliased C.signed_int;
      Socket_Address_Argument : C.sys.socket.CONST_SOCKADDR_ARG;
   begin
      declare
         Service_As_String : String (1 .. 5);
         for Service_As_String'Address use Service'Address;
         Service_Last : Natural;
         Error : Boolean;
      begin
         Formatting.Image (
            Formatting.Unsigned (Port),
            Service_As_String,
            Service_Last,
            Base => 10,
            Error => Error);
         Service_Length := C.size_t (Service_Last);
      end;
      Service (Service_Length) := C.char'Val (0);
      if C.netdb.getaddrinfo (
         null,
         Service (0)'Access,
         Hints'Access,
         Data'Access) /= 0
      then
         Server := Invalid_Listener; -- Use_Error
      else
         Server := C.sys.socket.socket (
            Data.ai_family,
            Data.ai_socktype,
            Data.ai_protocol);
         --  set FD_CLOEXEC
         Set_Close_On_Exec (Server);
         --  set SO_REUSEADDR
         Reuse_Addr_Option := 1;
         if C.sys.socket.setsockopt (
            Server,
            C.sys.socket.SOL_SOCKET,
            C.sys.socket.SO_REUSEADDR,
            C.void_const_ptr (Reuse_Addr_Option'Address),
            Reuse_Addr_Option'Size / Standard'Storage_Unit) < 0
         then
            Close_Listener (Server, Raise_On_Error => False);
            C.netdb.freeaddrinfo (Data);
            Server := Invalid_Listener; -- Use_Error
         end if;
         --  bind
         Socket_Address_Argument.sockaddr :=
            C.bits.socket.struct_sockaddr_const_ptr (Data.ai_addr);
         if C.sys.socket.bind (
            Server,
            Socket_Address_Argument,
            Data.ai_addrlen) < 0
         then
            Close_Listener (Server, Raise_On_Error => False);
            C.netdb.freeaddrinfo (Data);
            Server := Invalid_Listener; -- Use_Error
         end if;
         --  free
         C.netdb.freeaddrinfo (Data);
         --  listen
         if C.sys.socket.listen (Server, C.sys.socket.SOMAXCONN) < 0 then
            Close_Listener (Server, Raise_On_Error => False);
            Server := Invalid_Listener; -- Use_Error
         end if;
      end if;
   end Listen;

   procedure Accept_Socket (
      Server : Listener;
      Handle : aliased out Handle_Type;
      Remote_Address : out Socket_Address) is
   begin
      loop
         declare
            Len : aliased C.unistd.socklen_t :=
               Socket_Address'Size / Standard'Storage_Unit;
            Socket_Address_Argument : C.sys.socket.SOCKADDR_ARG;
            R : C.signed_int;
         begin
            Synchronous_Control.Unlock_Abort;
            Socket_Address_Argument.sockaddr :=
               Remote_Address'Unrestricted_Access;
            R := C.sys.socket.C_accept (
               Server,
               Socket_Address_Argument,
               Len'Access);
            Synchronous_Control.Lock_Abort;
            if R < 0 then
               if C.errno.errno /= C.errno.EINTR then
                  Handle := Invalid_Handle; -- Use_Error
                  exit;
               end if;
               --  interrupted and the signal is not "abort", then retry
            else
               Handle := R;
               Set_Close_On_Exec (Handle);
               exit;
            end if;
         end;
      end loop;
   end Accept_Socket;

   procedure Close_Listener (Server : Listener; Raise_On_Error : Boolean) is
      R : C.signed_int;
   begin
      R := C.unistd.close (Server);
      if R < 0 and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Listener;

end System.Native_IO.Sockets;
