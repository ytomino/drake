with System.Formatting;
with C.bits.socket;
with C.netinet.in_h;
with C.sys.socket;
with C.unistd;
package body System.Native_IO.Sockets is
   use type C.netdb.struct_addrinfo_ptr;
   use type C.size_t;

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
      Result : C.signed_int;
   begin
      Result := C.netdb.getaddrinfo (
         Host_Name,
         Service,
         Hints,
         Data'Access);
      if Result /= 0 then
         return null; -- Use_Error
      else
         return Data;
      end if;
   end Get;

   --  implementation

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
         ai_flags => 0, -- darwin9 does not have AI_NUMERICSERV
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

end System.Native_IO.Sockets;
