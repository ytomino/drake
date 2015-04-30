with Ada.Exception_Identification.From_Here;
with Ada.Streams.Stream_IO.Naked;
with System.Native_IO;
package body Ada.Streams.Stream_IO.Sockets is
   use Exception_Identification.From_Here;
   use type System.Native_IO.Handle_Type;
   use type System.Native_IO.Sockets.End_Point;

   function Get (
      Data : System.Native_IO.Sockets.End_Point)
      return End_Point;
   function Get (
      Data : System.Native_IO.Sockets.End_Point)
      return End_Point is
   begin
      if Data = null then
         Raise_Exception (Use_Error'Identity);
      else
         return Result : End_Point do
            Reference (Result).all := Data;
         end return;
      end if;
   end Get;

   --  implementation

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

   procedure Connect (File : in out File_Type; Peer : End_Point) is
      Handle : System.Native_IO.Handle_Type;
   begin
      System.Native_IO.Sockets.Connect (
         Handle,
         Reference (Peer).all);
      if Handle = System.Native_IO.Invalid_Handle then
         Raise_Exception (Use_Error'Identity);
      else
         Naked.Open (
            File,
            Append_File, -- Inout
            Handle,
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

      function Reference (
         Object : End_Point)
         return not null access System.Native_IO.Sockets.End_Point is
      begin
         return Object.Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out End_Point) is
      begin
         System.Native_IO.Sockets.Finalize (Object.Data);
      end Finalize;

   end End_Points;

end Ada.Streams.Stream_IO.Sockets;
