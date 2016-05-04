pragma License (Unrestricted);
--  extended unit
package Ada.Environment_Encoding.Encoding_Streams is
   --  This is a stream wrapper encoding from/to another stream.
   pragma Preelaborate;

   --  only reading

   type In_Type is limited private;

--  subtype Open_In_Type is In_Type
--    with
--       Dynamic_Predicate => Is_Open (Open_In_Type),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Object : In_Type) return Boolean;
   pragma Inline (Is_Open);

   --  management
   function Open (
      Decoder : Converter; -- Open_Converter
      Stream : not null access Streams.Root_Stream_Type'Class)
      return In_Type;
      --  The parameter Decoder is neither access nor aliased
      --    for derived types. The same applies hereafter.

   --  stream access
   function Stream (
      Object : aliased in out In_Type) -- Open_In_Type
      return not null access Streams.Root_Stream_Type'Class;

   --  only writing

   type Out_Type is limited private;

--  subtype Open_Out_Type is Out_Type
--    with
--       Dynamic_Predicate => Is_Open (Open_Out_Type),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Object : Out_Type) return Boolean;
   pragma Inline (Is_Open);

   --  management
   function Open (
      Encoder : Converter; -- Open_Converter
      Stream : not null access Streams.Root_Stream_Type'Class)
      return Out_Type;

   --  stream access
   function Stream (
      Object : aliased in out Out_Type) -- Open_Out_Type
      return not null access Streams.Root_Stream_Type'Class;

   --  finish writing
   procedure Finish (
      Object : in out Out_Type); -- Open_Out_Type

   --  bidirectional

   type Inout_Type is limited private;

--  subtype Open_Inout_Type is Out_Type
--    with
--       Dynamic_Predicate => Is_Open (Open_Inout_Type),
--       Predicate_Failure => raise Status_Error;

   function Is_Open (Object : Inout_Type) return Boolean;
   pragma Inline (Is_Open);

   --  management
   function Open (
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : not null access Streams.Root_Stream_Type'Class)
      return Inout_Type;

   --  substitute (encoded as internal)
   function Substitute (
      Object : Inout_Type) -- Open_Inout_Type
      return Streams.Stream_Element_Array;
   procedure Set_Substitute (
      Object : in out Inout_Type; -- Open_Inout_Type
      Substitute : Streams.Stream_Element_Array);

   --  stream access
   function Stream (
      Object : aliased in out Inout_Type) -- Open_Inout_Type
      return not null access Streams.Root_Stream_Type'Class;

   --  finish writing
   procedure Finish (
      Object : in out Inout_Type); -- Open_Inout_Type

private
   use type Streams.Stream_Element_Offset;

   Half_Buffer_Length : constant := 64;

   subtype Buffer_Type is
      Streams.Stream_Element_Array (0 .. 2 * Half_Buffer_Length - 1);

   type Reading_Status_Type is (Continuing, Finishing, Ended);
   pragma Discard_Names (Reading_Status_Type);

   type Reading_Context_Type is record
      Buffer : Buffer_Type;
      First : Streams.Stream_Element_Offset;
      Last : Streams.Stream_Element_Offset;
      Converted_Buffer : Buffer_Type;
      Converted_First : Streams.Stream_Element_Offset;
      Converted_Last : Streams.Stream_Element_Offset;
      Status : Reading_Status_Type;
   end record;
   pragma Suppress_Initialization (Reading_Context_Type);

   type Writing_Context_Type is record
      Buffer : Buffer_Type;
      First : Streams.Stream_Element_Offset;
      Last : Streams.Stream_Element_Offset;
   end record;
   pragma Suppress_Initialization (Writing_Context_Type);

   type Converter_Access is access constant Converter;
   for Converter_Access'Storage_Size use 0;

   --  only reading

   type In_Type is limited new Streams.Root_Stream_Type with record
      Stream : System.Address := -- access Root_Stream_Type'Class;
         System.Null_Address;
      Reading_Converter : Converter_Access;
      Reading_Context : Reading_Context_Type;
   end record;

   overriding procedure Read (
      Object : in out In_Type;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset);
   overriding procedure Write (
      Object : in out In_Type;
      Item : Streams.Stream_Element_Array)
      with Import, Convention => Ada, External_Name => "__drake_program_error";

   --  only writing

   type Out_Type is limited new Streams.Root_Stream_Type with record
      Stream : System.Address := -- access Root_Stream_Type'Class;
         System.Null_Address;
      Writing_Converter : Converter_Access;
      Writing_Context : Writing_Context_Type;
   end record;

   overriding procedure Read (
      Object : in out Out_Type;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset)
      with Import, Convention => Ada, External_Name => "__drake_program_error";
   overriding procedure Write (
      Object : in out Out_Type;
      Item : Streams.Stream_Element_Array);

   --  bidirectional

   type Inout_Type is limited new Streams.Root_Stream_Type with record
      Internal : Encoding_Id;
      External : Encoding_Id;
      Stream : System.Address := -- access Root_Stream_Type'Class;
         System.Null_Address;
      --  substitute (encoded as internal)
      Substitute_Length : Streams.Stream_Element_Offset;
      Substitute : Streams.Stream_Element_Array (1 .. Max_Substitute_Length);
      --  reading
      Reading_Converter : Converter;
      Reading_Context : Reading_Context_Type;
      --  writing
      Writing_Converter : Converter;
      Writing_Context : Writing_Context_Type;
   end record;

   overriding procedure Read (
      Object : in out Inout_Type;
      Item : out Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset);
   overriding procedure Write (
      Object : in out Inout_Type;
      Item : Streams.Stream_Element_Array);

end Ada.Environment_Encoding.Encoding_Streams;
