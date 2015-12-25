pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with System.Native_IO.Sockets;
package Ada.Streams.Stream_IO.Sockets is
   --  There are subprograms to create socket.
   pragma Preelaborate;

   type Port_Number is range 0 .. 16#ffff#;

   type Socket_Address is private;

   --  client

   type End_Point is limited private;

--  subtype Assigned_End_Point is End_Point
--    with
--       Dynamic_Predicate => Is_Assigned (Assigned_End_Point),
--       Predicate_Failure => raise Status_Error;

   function Is_Assigned (Peer : End_Point) return Boolean;

   function Resolve (Host_Name : String; Service : String)
      return End_Point;
   function Resolve (Host_Name : String; Port : Port_Number)
      return End_Point;

   procedure Connect (
      File : in out File_Type;
      Peer : End_Point); -- Assigned_End_Point
   function Connect (
      Peer : End_Point) -- Assigned_End_Point
      return File_Type;

   --  server

   type Listener is limited private;

--  subtype Open_Listener is Listener
--    with
--       Dynamic_Predicate => Is_Open (Assigned_Listener),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Server : Listener) return Boolean;

   function Listen (Port : Port_Number) return Listener;
   procedure Close (Server : in out Listener);

   procedure Accept_Socket (
      Server : Listener; -- Open_Listener
      File : in out File_Type);
   procedure Accept_Socket (
      Server : Listener; -- Open_Listener
      File : in out File_Type;
      Remote_Address : out Socket_Address);

private

   type Socket_Address is new System.Native_IO.Sockets.Socket_Address;

   package End_Points is

      type End_Point is limited private;

      function Reference (
         Object : End_Point)
         return not null access System.Native_IO.Sockets.End_Point;
      pragma Inline (Reference);

   private

      type End_Point is limited new Finalization.Limited_Controlled with record
         Data : aliased System.Native_IO.Sockets.End_Point := null;
      end record;

      overriding procedure Finalize (Object : in out End_Point);

   end End_Points;

   type End_Point is new End_Points.End_Point;

   package Listeners is

      type Listener is limited private;

      function Reference (Object : Listener)
         return not null access System.Native_IO.Sockets.Listener;
      pragma Inline (Reference);

   private

      type Listener is limited new Finalization.Limited_Controlled with record
         Data : aliased System.Native_IO.Sockets.Listener :=
            System.Native_IO.Sockets.Invalid_Listener;
      end record;

      overriding procedure Finalize (Object : in out Listener);

   end Listeners;

   type Listener is new Listeners.Listener;

end Ada.Streams.Stream_IO.Sockets;
