with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Formatting;
with System.Synchronous_Control;
with C.errno;
with C.netinet.in_h;
with C.unistd;
package body System.Native_IO.Sockets is
   use Ada.Exception_Identification.From_Here;
   use type C.size_t;
   use type C.netdb.struct_addrinfo_ptr;
   use type C.sys.socket.socklen_t;

   --  implementation

   procedure Close_Socket (Handle : Handle_Type; Raise_On_Error : Boolean) is
      R : C.signed_int;
   begin
      R := C.unistd.close (Handle);
      if R < 0 and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity);
      end if;
   end Close_Socket;

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
         Raise_Exception (Use_Error'Identity);
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
         ai_socktype => C.sys.socket.SOCK_STREAM,
         ai_protocol => C.netinet.in_h.IPPROTO_TCP,
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
         ai_flags => 0, -- darwin9 does not have AI_NUMERICSERV
         ai_family => C.sys.socket.AF_UNSPEC,
         ai_socktype => C.sys.socket.SOCK_STREAM,
         ai_protocol => C.netinet.in_h.IPPROTO_TCP,
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
            Formatting.Word_Unsigned (Port),
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
         Handle :=
            C.sys.socket.socket (I.ai_family, I.ai_socktype, I.ai_protocol);
         if Handle >= 0 then
            if C.sys.socket.connect (Handle, I.ai_addr, I.ai_addrlen) = 0 then
               --  connected
               Set_Close_On_Exec (Handle);
               return;
            end if;
            declare
               Closing_Handle : constant Handle_Type := Handle;
            begin
               Handle := Invalid_Handle;
               Close_Socket (Closing_Handle, Raise_On_Error => True);
            end;
         end if;
         I := I.ai_next;
      end loop;
      Raise_Exception (Use_Error'Identity);
   end Connect;

   procedure Finalize (Item : End_Point) is
   begin
      C.netdb.freeaddrinfo (Item);
   end Finalize;

   --  implementation of server

   procedure Listen (Server : aliased out Listener; Port : Port_Number) is
      Hints : aliased constant C.netdb.struct_addrinfo := (
         ai_flags => C.netdb.AI_PASSIVE, -- or AI_NUMERICSERV
         ai_family => C.sys.socket.AF_UNSPEC,
         ai_socktype => C.sys.socket.SOCK_STREAM,
         ai_protocol => C.netinet.in_h.IPPROTO_TCP,
         ai_addrlen => 0,
         ai_canonname => null,
         ai_addr => null,
         ai_next => null);
      Data : aliased C.netdb.struct_addrinfo_ptr;
      Service : C.char_array (0 .. 5); -- "65535" & NUL
      Service_Length : C.size_t;
   begin
      declare
         Service_As_String : String (1 .. 5);
         for Service_As_String'Address use Service'Address;
         Service_Last : Natural;
         Error : Boolean;
      begin
         Formatting.Image (
            Formatting.Word_Unsigned (Port),
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
         Raise_Exception (Use_Error'Identity);
      end if;
      declare
         procedure Finally (X : in out C.netdb.struct_addrinfo_ptr);
         procedure Finally (X : in out C.netdb.struct_addrinfo_ptr) is
         begin
            C.netdb.freeaddrinfo (X);
         end Finally;
         package Holder is
            new Ada.Exceptions.Finally.Scoped_Holder (
               C.netdb.struct_addrinfo_ptr,
               Finally);
         Reuse_Addr_Option : aliased C.signed_int;
      begin
         Holder.Assign (Data);
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
            Raise_Exception (Use_Error'Identity);
         end if;
         --  bind
         if C.sys.socket.bind (Server, Data.ai_addr, Data.ai_addrlen) < 0 then
            Raise_Exception (Use_Error'Identity);
         end if;
         --  listen
         if C.sys.socket.listen (Server, C.sys.socket.SOMAXCONN) < 0 then
            Raise_Exception (Use_Error'Identity);
         end if;
      end;
   end Listen;

   procedure Accept_Socket (
      Server : Listener;
      Handle : aliased out Handle_Type;
      Remote_Address : out Socket_Address) is
   begin
      loop
         declare
            Len : aliased C.sys.socket.socklen_t :=
               Socket_Address'Size / Standard'Storage_Unit;
            R : C.signed_int;
            errno : C.signed_int;
         begin
            Synchronous_Control.Unlock_Abort;
            R := C.sys.socket.C_accept (
               Server,
               Remote_Address'Unrestricted_Access,
               Len'Access);
            errno := C.errno.errno;
            Synchronous_Control.Lock_Abort;
            if R < 0 then
               if errno /= C.errno.EINTR then
                  Raise_Exception (Use_Error'Identity);
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

end System.Native_IO.Sockets;
