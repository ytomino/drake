pragma License (Unrestricted);
--  extended unit
package System.Native_Encoding.Encoding_Streams is
   --  This is a stream wrapper encoding from/to another stream.
   pragma Preelaborate;

   type Encoding is limited private;

   --  management
   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute)
      return Encoding;
   function Is_Open (Object : Encoding) return Boolean;

   --  stream access
   function Stream (Object : aliased in out Encoding)
      return not null access Ada.Streams.Root_Stream_Type'Class;

   --  exceptions

   End_Error : exception
      renames Ada.IO_Exceptions.End_Error;

private
   use type Ada.Streams.Stream_Element_Offset;

   type Encoding is limited new Ada.Streams.Root_Stream_Type with record
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Substitute : Ada.Streams.Stream_Element;
      --  reading
      Reading_Converter : Converter;
      Reading_Buffer : Ada.Streams.Stream_Element_Array (0 .. Expanding - 1);
      Reading_Last : Ada.Streams.Stream_Element_Offset;
      --  writing
      Writing_Converter : Converter;
      Writing_Buffer : Ada.Streams.Stream_Element_Array (0 .. Expanding - 1);
      Writing_Last : Ada.Streams.Stream_Element_Offset;
   end record;

   overriding procedure Read (
      Object : in out Encoding;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   overriding procedure Write (
      Object : in out Encoding;
      Item : Ada.Streams.Stream_Element_Array);

end System.Native_Encoding.Encoding_Streams;
