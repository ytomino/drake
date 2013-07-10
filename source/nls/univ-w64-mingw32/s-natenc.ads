pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with C.windef;
private with C.winnls;
package System.Native_Encoding is
   --  Platform-depended text encoding.
   pragma Preelaborate;

   Default_Substitute : constant := Character'Pos ('?');

   type Encoding_Id is private;

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

   --  exceptions

   Name_Error : exception
      renames Ada.IO_Exceptions.Name_Error;
   Status_Error : exception
      renames Ada.IO_Exceptions.Status_Error;

private
   use type C.char_array;

   type Encoding_Id is new C.windef.UINT;

   UTF_8 : constant Encoding_Id := C.winnls.CP_UTF8;
   UTF_16 : constant Encoding_Id := 16#ffff_ff10#; -- dummy value
   UTF_32 : constant Encoding_Id := 16#ffff_ff20#; -- dummy value

   Invalid_Encoding_Id : constant := 16#ffff_ffff#; -- dummy value

   type Converter is record
      From : Encoding_Id := Invalid_Encoding_Id;
      To : Encoding_Id;
   end record;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

   procedure Open (Object : out Converter; From, To : Encoding_Id);

   --  max length of one multi-byte character

   Expanding : constant := 6; -- UTF-8

end System.Native_Encoding;
