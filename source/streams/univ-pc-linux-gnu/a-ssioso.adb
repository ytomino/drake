with Ada.Streams.Stream_IO.Inside;
with Ada.Exceptions;
with System.Formatting;
with C.bits.socket;
with C.netinet.in_h;
with C.sys.socket;
with C.unistd;
package body Ada.Streams.Stream_IO.Sockets is
   use type C.signed_int;
   use type C.netdb.struct_addrinfo_ptr;

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
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      else
         return Result : End_Point do
            Assign (Result, Data);
         end return;
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
      Z_Host_Name : String := Host_Name & Character'Val (0);
      C_Host_Name : C.char_array (C.size_t);
      for C_Host_Name'Address use Z_Host_Name'Address;
      Z_Service : String := Service & Character'Val (0);
      C_Service : C.char_array (C.size_t);
      for C_Service'Address use Z_Service'Address;
   begin
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
      Z_Host_Name : String := Host_Name & Character'Val (0);
      C_Host_Name : C.char_array (C.size_t);
      for C_Host_Name'Address use Z_Host_Name'Address;
      Z_Service : String (1 .. 5);
      Z_Service_Last : Natural;
      C_Service : C.char_array (C.size_t);
      for C_Service'Address use Z_Service'Address;
      Error : Boolean;
   begin
      System.Formatting.Image (
         System.Formatting.Unsigned (Port),
         Z_Service,
         Z_Service_Last,
         Base => 10,
         Error => Error);
      Z_Service (Z_Service_Last + 1) := Character'Val (0);
      return Get (C_Host_Name (0)'Access, C_Service (0)'Access, Hints'Access);
   end Resolve;

   procedure Connect (File : in out File_Type; Peer : End_Point) is
      Handle : Inside.Handle_Type := -1;
      I : C.netdb.struct_addrinfo_ptr := Reference (Peer);
   begin
      while I /= null loop
         Handle := C.sys.socket.socket (
            I.ai_family,
            I.ai_socktype,
            I.ai_protocol);
         if Handle >= 0 then
            exit when C.sys.socket.connect (
               Handle,
               C.sys.socket.CONST_SOCKADDR_ARG'(
                  Unchecked_Tag => 0,
                  sockaddr => C.bits.socket.struct_sockaddr_const_ptr (
                     I.ai_addr)),
               I.ai_addrlen) = 0;
            declare
               Dummy : C.signed_int;
               pragma Unreferenced (Dummy);
            begin
               Dummy := C.unistd.close (Handle);
            end;
         end if;
         I := I.ai_next;
      end loop;
      if Handle < 0 then
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      else
         Inside.Set_Close_On_Exec (Handle);
         Inside.Open (
            File,
            Handle,
            Append_File, -- Inout
            To_Close => True);
      end if;
   end Connect;

   function Connect (Peer : End_Point) return File_Type is
   begin
      return Result : File_Type do
         Connect (Result, Peer);
      end return;
   end Connect;

   package body End_Points is

      procedure Assign (
         Object : in out End_Point;
         Data : C.netdb.struct_addrinfo_ptr) is
      begin
         Object.Data := Data;
      end Assign;

      function Reference (
         Object : End_Point)
         return C.netdb.struct_addrinfo_ptr is
      begin
         return Object.Data;
      end Reference;

      overriding procedure Finalize (Object : in out End_Point) is
      begin
         C.netdb.freeaddrinfo (Object.Data);
      end Finalize;

   end End_Points;

end Ada.Streams.Stream_IO.Sockets;
