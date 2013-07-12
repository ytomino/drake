pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with Ada.Finalization;
private with C.errno;
private with C.iconv;
package System.Native_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;

   type Encoding_Id is private;

   Default_Substitute : constant := Character'Pos ('?');

   UTF_8 : constant Encoding_Id;
   UTF_16 : constant Encoding_Id;
   UTF_32 : constant Encoding_Id;

   type Error_Status is (Fine, Incomplete, Illegal_Sequence);

   --  converter

   type Converter is limited private;

   function Is_Open (Object : Converter) return Boolean;

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute);

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;

private
   use type C.char_array;

   --  max length of one multi-byte character

   Expanding : constant := 6; -- UTF-8

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

   for Error_Status use (
      Fine => 0,
      Incomplete => C.errno.EINVAL,
      Illegal_Sequence => C.errno.EILSEQ);

   package Controlled is

      type Converter is limited private;

      procedure Open (Object : out Converter; From, To : Encoding_Id);

      function Value (Object : Converter) return C.iconv.iconv_t;
      pragma Inline (Value);

   private

      type Converter is
         limited new Ada.Finalization.Limited_Controlled with
      record
         iconv : C.iconv.iconv_t := C.iconv.iconv_t (Null_Address);
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
      Status : out Error_Status);

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute);

end System.Native_Encoding;
