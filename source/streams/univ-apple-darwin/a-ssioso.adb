with Ada.Streams.Stream_IO.Inside;
with System.Formatting;
with C.netinet.in_h;
with C.sys.fcntl;
with C.sys.socket;
with C.unistd;
package body Ada.Streams.Stream_IO.Sockets is
   use type C.signed_int;
   use type C.netdb.struct_addrinfo_ptr;

   function Get (
      Host_Name : not null access constant C.char;
      Service : not null access constant C.char;
      Hints : not null access constant C.netdb.struct_addrinfo)
      return Address_Information;
   function Get (
      Host_Name : not null access constant C.char;
      Service : not null access constant C.char;
      Hints : not null access constant C.netdb.struct_addrinfo)
      return Address_Information
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
         raise Use_Error;
      else
         return (Finalization.Limited_Controlled with Data);
      end if;
   end Get;

   --  implementation

   function Get (Host_Name : String; Service : String)
      return Address_Information
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
      Z_Host_Name : String := Host_Name & Character'Val (0);
      C_Host_Name : C.char_array (C.size_t);
      for C_Host_Name'Address use Z_Host_Name'Address;
      Z_Service : String := Service & Character'Val (0);
      C_Service : C.char_array (C.size_t);
      for C_Service'Address use Z_Service'Address;
   begin
      return Get (C_Host_Name (0)'Access, C_Service (0)'Access, Hints'Access);
   end Get;

   function Get (Host_Name : String; Port : Port_Number)
      return Address_Information
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
   end Get;

   procedure Connect (File : in out File_Type; Host : Address_Information) is
      Handle : Inside.Handle_Type := -1;
      I : C.netdb.struct_addrinfo_ptr := Host.Data;
   begin
      while I /= null loop
         Handle := C.sys.socket.socket (
            Host.Data.ai_family,
            Host.Data.ai_socktype,
            Host.Data.ai_protocol);
         if Handle >= 0 then
            exit when C.sys.socket.connect (
               Handle,
               Host.Data.ai_addr,
               Host.Data.ai_addrlen) = 0;
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
         raise Use_Error;
      else
         declare
            Dummy : C.signed_int;
            pragma Unreferenced (Dummy);
         begin
            Dummy := C.sys.fcntl.fcntl (
               Handle,
               C.sys.fcntl.F_SETFD,
               C.sys.fcntl.FD_CLOEXEC);
         end;
         Inside.Open (
            File,
            Handle,
            Append_File, -- Inout
            To_Close => True);
      end if;
   end Connect;

   function Connect (Host : Address_Information) return File_Type is
   begin
      return Result : File_Type do
         Connect (Result, Host);
      end return;
   end Connect;

   overriding procedure Finalize (Object : in out Address_Information) is
   begin
      C.netdb.freeaddrinfo (Object.Data);
   end Finalize;

end Ada.Streams.Stream_IO.Sockets;
