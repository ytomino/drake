pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams;
private with C.windef;
private with C.winnls;
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

   type Substituting_Status_Type is new Status_Type range Fine .. Insufficient;

   --  converter

   type Converter is limited private;

   function Is_Open (Object : Converter) return Boolean;

   function Min_Size_In_From_Stream_Elements (Object : Converter)
      return Ada.Streams.Stream_Element_Offset;

   function Substitute (Object : Converter)
      return Ada.Streams.Stream_Element_Array;

   procedure Set_Substitute (
      Object : in out Converter;
      Substitute : Ada.Streams.Stream_Element_Array);

   --  convert one character sequence
   procedure Convert (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type);

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

private
   use type Ada.Streams.Stream_Element_Offset;
   use type C.char_array;

   --  max length of one multi-byte character

   Max_Substitute_Length : constant := 6; -- UTF-8

   --  encoding identifier

   type Encoding_Id is new C.windef.UINT;

   UTF_8 : constant Encoding_Id := C.winnls.CP_UTF8;
   UTF_16 : constant Encoding_Id := 16#ffff_ff10#; -- dummy value
   UTF_32 : constant Encoding_Id := 16#ffff_ff20#; -- dummy value

   Invalid_Encoding_Id : constant := 16#ffff_ffff#; -- dummy value

   --  converter

   type Converter is record
      From : Encoding_Id := Invalid_Encoding_Id;
      To : Encoding_Id;
      Substitute_Length : Ada.Streams.Stream_Element_Offset;
      Substitute : aliased Ada.Streams.Stream_Element_Array (
         1 ..
         Max_Substitute_Length + 1); -- zero terminated
   end record;

   procedure Convert_No_Check (
      Object : Converter;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type);

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

   procedure Open (Object : out Converter; From, To : Encoding_Id);

end System.Native_Encoding;
