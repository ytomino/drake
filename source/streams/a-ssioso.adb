with Ada.Exceptions.Finally;
with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
package body Ada.Streams.Stream_IO.Sockets is
   use type System.Native_IO.Sockets.End_Point;
--  use type System.Native_IO.Handle_Type;
--  use type System.Native_IO.Sockets.Listener;

   procedure Socket_Finally (X : in out System.Native_IO.Handle_Type);
   procedure Socket_Finally (X : in out System.Native_IO.Handle_Type) is
      use type System.Native_IO.Handle_Type;
   begin
      if X /= System.Native_IO.Invalid_Handle then
         System.Native_IO.Sockets.Close_Socket (
            X,
            Raise_On_Error => False);
      end if;
   end Socket_Finally;

   procedure Listener_Finally (X : in out System.Native_IO.Sockets.Listener);
   procedure Listener_Finally (X : in out System.Native_IO.Sockets.Listener) is
      use type System.Native_IO.Sockets.Listener;
   begin
      if X /= System.Native_IO.Sockets.Invalid_Listener then
         System.Native_IO.Sockets.Close_Listener (
            X,
            Raise_On_Error => False);
      end if;
   end Listener_Finally;

   --  client

   function Get (Data : System.Native_IO.Sockets.End_Point) return End_Point;
   function Get (Data : System.Native_IO.Sockets.End_Point) return End_Point is
   begin
      return Result : End_Point do
         declare
            Result_Handle : System.Native_IO.Sockets.End_Point
               renames Controlled_End_Points.Reference (Result).all;
         begin
            Result_Handle := Data;
         end;
      end return;
   end Get;

   --  implementation of client

   function Is_Assigned (Peer : End_Point) return Boolean is
      Peer_Handle : System.Native_IO.Sockets.End_Point
         renames Controlled_End_Points.Reference (Peer).all;
   begin
      return Peer_Handle /= null;
   end Is_Assigned;

   function Resolve (Host_Name : String; Service : String)
      return End_Point
   is
      Peer : constant System.Native_IO.Sockets.End_Point :=
         System.Native_IO.Sockets.Resolve (Host_Name, Service);
   begin
      return Get (Peer);
   end Resolve;

   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point
   is
      Peer : constant System.Native_IO.Sockets.End_Point :=
         System.Native_IO.Sockets.Resolve (
            Host_Name,
            System.Native_IO.Sockets.Port_Number (Port));
   begin
      return Get (Peer);
   end Resolve;

   procedure Connect (
      File : in out File_Type;
      Peer : End_Point)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Peer) or else raise Status_Error);
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            System.Native_IO.Handle_Type,
            Socket_Finally);
      Handle : aliased System.Native_IO.Handle_Type :=
         System.Native_IO.Invalid_Handle;
   begin
      Holder.Assign (Handle);
      declare
         Peer_Handle : System.Native_IO.Sockets.End_Point
            renames Controlled_End_Points.Reference (Peer).all;
      begin
         System.Native_IO.Sockets.Connect (Handle, Peer_Handle);
      end;
      Naked.Open (
         File,
         Append_File, -- Inout
         Handle,
         To_Close => True);
      --  complete
      Holder.Clear;
   end Connect;

   function Connect (
      Peer : End_Point)
      return File_Type is
   begin
      return Result : File_Type do
         Connect (
            Result,
            Peer); -- checking the predicate
      end return;
   end Connect;

   package body Controlled_End_Points is

      function Reference (Object : Sockets.End_Point)
         return not null access System.Native_IO.Sockets.End_Point is
      begin
         return End_Point (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out End_Point) is
      begin
         System.Native_IO.Sockets.Finalize (Object.Data);
      end Finalize;

   end Controlled_End_Points;

   --  implementation of server

   function Is_Open (Server : Listener) return Boolean is
      use type System.Native_IO.Sockets.Listener;
      Server_Handle : System.Native_IO.Sockets.Listener
         renames Controlled_Listeners.Reference (Server).all;
   begin
      return Server_Handle /= System.Native_IO.Sockets.Invalid_Listener;
   end Is_Open;

   function Listen (Port : Port_Number) return Listener is
   begin
      return Result : Listener do
         declare
            package Holder is
               new Exceptions.Finally.Scoped_Holder (
                  System.Native_IO.Sockets.Listener,
                  Listener_Finally);
            Result_Handle : System.Native_IO.Sockets.Listener
               renames Controlled_Listeners.Reference (Result).all;
         begin
            Holder.Assign (Result_Handle);
            System.Native_IO.Sockets.Listen (
               Result_Handle,
               System.Native_IO.Sockets.Port_Number (Port));
            --  complete
            Holder.Clear;
         end;
      end return;
   end Listen;

   procedure Close (Server : in out Listener) is
      pragma Check (Pre,
         Check => Is_Open (Server) or else raise Status_Error);
      Server_Handle : System.Native_IO.Sockets.Listener
         renames Controlled_Listeners.Reference (Server).all;
      Freeing_Handle : constant System.Native_IO.Sockets.Listener :=
         Server_Handle;
   begin
      Server_Handle := System.Native_IO.Sockets.Invalid_Listener;
      System.Native_IO.Sockets.Close_Listener (
         Freeing_Handle,
         Raise_On_Error => True);
   end Close;

   procedure Accept_Socket (
      Server : Listener;
      File : in out File_Type)
   is
      Dummy : Socket_Address;
   begin
      Accept_Socket (
         Server, -- checking the predicate
         File,
         Dummy);
   end Accept_Socket;

   procedure Accept_Socket (
      Server : Listener;
      File : in out File_Type;
      Remote_Address : out Socket_Address)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Server) or else raise Status_Error);
      package Holder is
         new Exceptions.Finally.Scoped_Holder (
            System.Native_IO.Handle_Type,
            Socket_Finally);
      Handle : aliased System.Native_IO.Handle_Type :=
         System.Native_IO.Invalid_Handle;
   begin
      Holder.Assign (Handle);
      declare
         Server_Handle : System.Native_IO.Sockets.Listener
            renames Controlled_Listeners.Reference (Server).all;
      begin
         System.Native_IO.Sockets.Accept_Socket (
            Server_Handle,
            Handle,
            System.Native_IO.Sockets.Socket_Address (Remote_Address));
      end;
      Naked.Open (
         File,
         Append_File, -- Inout
         Handle,
         To_Close => True);
      --  complete
      Holder.Clear;
   end Accept_Socket;

   package body Controlled_Listeners is

      function Reference (Object : Sockets.Listener)
         return not null access System.Native_IO.Sockets.Listener is
      begin
         return Listener (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Listener) is
      begin
         Listener_Finally (Object.Data);
      end Finalize;

   end Controlled_Listeners;

end Ada.Streams.Stream_IO.Sockets;
