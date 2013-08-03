pragma License (Unrestricted);
--  extended unit
package System.Native_Encoding.Encoding_Streams is
   --  This is a stream wrapper encoding from/to another stream.
   pragma Preelaborate;

   --  only reading

   type In_Type is limited private;

   --  management
   function Open (
      Decoder : Converter; -- neither access nor aliased for derived types
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return In_Type;
   function Is_Open (Object : In_Type) return Boolean;

   --  stream access
   function Stream (Object : aliased in out In_Type)
      return not null access Ada.Streams.Root_Stream_Type'Class;

   --  only writing

   type Out_Type is limited private;

   --  management
   function Open (
      Encoder : Converter; -- same as above
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Out_Type;
   function Is_Open (Object : Out_Type) return Boolean;

   --  stream access
   function Stream (Object : aliased in out Out_Type)
      return not null access Ada.Streams.Root_Stream_Type'Class;

   --  finish writing
   procedure Finish (Object : in out Out_Type);

   --  bidirectional

   type Inout_Type is limited private;

   --  management
   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Inout_Type;
   function Is_Open (Object : Inout_Type) return Boolean;

   --  substitute (encoded as internal)
   function Substitute (Object : Inout_Type)
      return Ada.Streams.Stream_Element_Array;
   procedure Set_Substitute (
      Object : in out Inout_Type;
      Substitute : Ada.Streams.Stream_Element_Array);

   --  stream access
   function Stream (Object : aliased in out Inout_Type)
      return not null access Ada.Streams.Root_Stream_Type'Class;

   --  finish writing
   procedure Finish (Object : in out Inout_Type);

   --  exceptions

   End_Error : exception
      renames Ada.IO_Exceptions.End_Error;

private

   Half_Buffer_Length : constant := 64;

   subtype Buffer_Type is
      Ada.Streams.Stream_Element_Array (0 .. 2 * Half_Buffer_Length - 1);

   type Reading_Status_Type is (Continuing, Finishing, Ended);
   pragma Discard_Names (Reading_Status_Type);

   type Reading_Context_Type is record
      Buffer : Buffer_Type;
      First : Ada.Streams.Stream_Element_Offset;
      Last : Ada.Streams.Stream_Element_Offset;
      Converted_Buffer : Buffer_Type;
      Converted_First : Ada.Streams.Stream_Element_Offset;
      Converted_Last : Ada.Streams.Stream_Element_Offset;
      Status : Reading_Status_Type;
   end record;
   pragma Suppress_Initialization (Reading_Context_Type);

   type Writing_Context_Type is record
      Buffer : Buffer_Type;
      First : Ada.Streams.Stream_Element_Offset;
      Last : Ada.Streams.Stream_Element_Offset;
   end record;
   pragma Suppress_Initialization (Writing_Context_Type);

   type Converter_Access is access constant Converter;
   for Converter_Access'Storage_Size use 0;

   --  only reading

   type In_Type is limited new Ada.Streams.Root_Stream_Type with record
      Stream : Address := Null_Address; -- access Root_Stream_Type'Class;
      Reading_Converter : Converter_Access;
      Reading_Context : Reading_Context_Type;
   end record;

   procedure Missing_Write (
      Object : in out In_Type;
      Item : Ada.Streams.Stream_Element_Array);
   pragma Import (Ada, Missing_Write, "__drake_program_error");

   overriding procedure Read (
      Object : in out In_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   overriding procedure Write (
      Object : in out In_Type;
      Item : Ada.Streams.Stream_Element_Array)
      renames Missing_Write;

   --  only writing

   type Out_Type is limited new Ada.Streams.Root_Stream_Type with record
      Stream : Address := Null_Address; -- access Root_Stream_Type'Class;
      Writing_Converter : Converter_Access;
      Writing_Context : Writing_Context_Type;
   end record;

   procedure Missing_Read (
      Object : in out Out_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   pragma Import (Ada, Missing_Read, "__drake_program_error");

   overriding procedure Read (
      Object : in out Out_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
      renames Missing_Read;
   overriding procedure Write (
      Object : in out Out_Type;
      Item : Ada.Streams.Stream_Element_Array);

   --  bidirectional

   type Inout_Type is limited new Ada.Streams.Root_Stream_Type with record
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : Address := Null_Address; -- access Root_Stream_Type'Class;
      --  substitute (encoded as internal)
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element_Array (
         1 ..
         Max_Substitute_Length);
      --  reading
      Reading_Converter : Converter;
      Reading_Context : Reading_Context_Type;
      --  writing
      Writing_Converter : Converter;
      Writing_Context : Writing_Context_Type;
   end record;

   overriding procedure Read (
      Object : in out Inout_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   overriding procedure Write (
      Object : in out Inout_Type;
      Item : Ada.Streams.Stream_Element_Array);

end System.Native_Encoding.Encoding_Streams;
