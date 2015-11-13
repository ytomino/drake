pragma License (Unrestricted);
--  extended unit
private with Ada.Finalization;
private with System.Native_IO.Sockets;
package Ada.Streams.Stream_IO.Sockets is
   --  There are subprograms to create socket.
   pragma Preelaborate;

   type Port_Number is range 0 .. 16#ffff#;

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

private

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

end Ada.Streams.Stream_IO.Sockets;
