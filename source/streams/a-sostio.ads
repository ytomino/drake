pragma License (Unrestricted);
--  extended unit
with System.Storage_Elements;
package Ada.Streams.Overlaps_Storage_IO is
   --  Overlapping stream access to existing memory.
   pragma Preelaborate;

   pragma Compile_Time_Error (
      Standard'Storage_Unit /= Stream_Element'Size,
      "this is not 8-bit machine.");

   type Overlay (<>) is limited private;

   function Create (
      Address : System.Address;
      Size : System.Storage_Elements.Storage_Count)
      return Overlay;

   procedure Reset (Object : in out Overlay);

   function Stream (Object : Overlay)
      return not null access Root_Stream_Type'Class;
   pragma Inline (Stream);

   --  Note: Write propagates Storage_Error if overflow.

private

   type Overlay is limited new Seekable_Stream_Type with record
      Address : System.Address;
      Size : System.Storage_Elements.Storage_Count;
      Index : System.Storage_Elements.Storage_Offset := 1;
   end record;

   overriding procedure Read (
      Object : in out Overlay;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);
   overriding procedure Write (
      Object : in out Overlay;
      Item : Stream_Element_Array);
   overriding procedure Set_Index (
      Object : in out Overlay;
      To : Stream_Element_Positive_Count);
   overriding function Index (Object : Overlay)
      return Stream_Element_Positive_Count;
   overriding function Size (Object : Overlay)
      return Stream_Element_Count;

end Ada.Streams.Overlaps_Storage_IO;
