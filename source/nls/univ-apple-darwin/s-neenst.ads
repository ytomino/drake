pragma License (Unrestricted);
--  extended unit
package System.Native_Encoding.Encoding_Streams is
   --  This is a stream wrapper encoding from/to another stream.
   pragma Preelaborate;

   type Encoding is limited private;

   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Encoding;

   function Is_Open (Object : Encoding) return Boolean;

   --  substitute (encoded as internal)

   function Substitute (Object : Encoding)
      return Ada.Streams.Stream_Element_Array;

   procedure Set_Substitute (
      Object : in out Encoding;
      Substitute : Ada.Streams.Stream_Element_Array);

   --  stream access

   function Stream (Object : aliased in out Encoding)
      return not null access Ada.Streams.Root_Stream_Type'Class;

   --  exceptions

   End_Error : exception
      renames Ada.IO_Exceptions.End_Error;

private

   Half_Buffer_Length : constant := 64;

   subtype Buffer_Type is
      Ada.Streams.Stream_Element_Array (0 .. 2 * Half_Buffer_Length - 1);

   type Encoding is limited new Ada.Streams.Root_Stream_Type with record
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      --  substitute (encoded as internal)
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element_Array (
         1 ..
         Max_Substitute_Length);
      --  reading
      Reading_Converter : Converter;
      Reading_Buffer : Buffer_Type;
      Reading_First : Ada.Streams.Stream_Element_Offset;
      Reading_Last : Ada.Streams.Stream_Element_Offset;
      Reading_Converted_Buffer : Buffer_Type;
      Reading_Converted_First : Ada.Streams.Stream_Element_Offset;
      Reading_Converted_Last : Ada.Streams.Stream_Element_Offset;
      --  writing
      Writing_Converter : Converter;
      Writing_Buffer : Buffer_Type;
      Writing_First : Ada.Streams.Stream_Element_Offset;
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
