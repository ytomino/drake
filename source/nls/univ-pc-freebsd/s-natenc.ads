pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with Ada.Finalization;
private with C.iconv;
package System.Native_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;

   --  encoding identifier

   type Encoding_Id is private;

   function Default_Substitute (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Array;

   function Min_Size_In_Stream_Elements (Encoding : Encoding_Id)
      return Ada.Streams.Stream_Element_Offset;

   UTF_8 : constant Encoding_Id;
   UTF_16 : constant Encoding_Id;
   UTF_32 : constant Encoding_Id;

   type Status_Type is (
      Fine,
      Insufficient, -- the output buffer is not large enough
      Incomplete, -- the input buffer is broken off at a multi-byte character
      Illegal_Sequence); -- a input character could not be mapped to the output

   type Finishing_Status_Type is new Status_Type range Fine .. Insufficient;
   type Substituting_Status_Type is new Status_Type range Fine .. Insufficient;

   --  converter

   type Converter is limited private;

   function Is_Open (Object : Converter) return Boolean;

   function Min_Size_In_From_Stream_Elements (Object : Converter)
      return Ada.Streams.Stream_Element_Offset;

   function Substitute (Object : Converter)
      return Ada.Streams.Stream_Element_Array;

   procedure Set_Substitute (
      Object : Converter;
      Substitute : Ada.Streams.Stream_Element_Array);

   --  convert subsequence
   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type);

   --  finish converting and receive remaindered sequence
   procedure Convert (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Finishing_Status_Type);

   --  convert all character sequence with substitute,
   --    and stop if Out_Item is not large enough
   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Substituting_Status_Type);

   --  convert all character sequence with substitute,
   --    and raise Constraint_Error if Out_Item is not large enough
   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset);

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;
   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;

private
   use type Ada.Streams.Stream_Element_Offset;
   use type C.char_array;

   --  max length of one multi-byte character

   Max_Substitute_Length : constant := 6; -- UTF-8

   --  encoding identifier

   type Encoding_Id is access constant C.char;
   for Encoding_Id'Storage_Size use 0;

   UTF_8_Name : aliased constant C.char_array (0 .. 5) :=
      "UTF-8" & C.char'Val (0);
   UTF_8 : constant Encoding_Id := UTF_8_Name (0)'Access;

   UTF_16_Names : aliased constant
      array (Bit_Order) of aliased C.char_array (0 .. 8) := (
         High_Order_First => "UTF-16BE" & C.char'Val (0),
         Low_Order_First => "UTF-16LE" & C.char'Val (0));
   UTF_16 : constant Encoding_Id := UTF_16_Names (Default_Bit_Order)(0)'Access;

   UTF_32_Names : aliased constant
      array (Bit_Order) of aliased C.char_array (0 .. 8) := (
         High_Order_First => "UTF-32BE" & C.char'Val (0),
         Low_Order_First => "UTF-32LE" & C.char'Val (0));
   UTF_32 : constant Encoding_Id := UTF_32_Names (Default_Bit_Order)(0)'Access;

   --  converter

   type Non_Controlled_Converter is record
      iconv : C.iconv.iconv_t;
      --  about "From"
      Min_Size_In_From_Stream_Elements : Ada.Streams.Stream_Element_Offset;
      --  about "To"
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element_Array (
         1 ..
         Max_Substitute_Length);
   end record;
   pragma Suppress_Initialization (Non_Controlled_Converter);

   package Controlled is

      type Converter is limited private;

      procedure Open (Object : out Converter; From, To : Encoding_Id);

      function Reference (Object : Converter)
         return not null access Non_Controlled_Converter;
      pragma Inline (Reference);

   private

      type Converter is
         limited new Ada.Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_Converter := (
            iconv => C.iconv.iconv_t (Null_Address),
            others => <>);
      end record;

      overriding procedure Finalize (Object : in out Converter);

   end Controlled;

   type Converter is new Controlled.Converter;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type);

   procedure Convert_No_Check (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Finishing_Status_Type);

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Substituting_Status_Type);

   procedure Put_Substitute (
      Object : Converter;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset);

end System.Native_Encoding;
