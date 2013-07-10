pragma License (Unrestricted);
--  extended unit
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
package System.Native_Encoding.Generic_Strings is
   --  Subprograms encode/decode text between string types
   --    and Ada.Streams.Stream_Element_Array.
   pragma Preelaborate;

   --  decoder

   type Decoder is new Converter;

   function From (Id : Encoding_Id) return Decoder;

   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Error_Status);

   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Substitute : Character_Type :=
         Character_Type'Val (Default_Substitute));

   function Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Substitute : Character_Type := Character_Type'Val (Default_Substitute))
      return String_Type;

   --  encoder

   type Encoder is new Converter;

   function To (Id : Encoding_Id) return Encoder;

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute);

   function Encode (
      Object : Encoder;
      Item : String_Type;
      Substitute : Ada.Streams.Stream_Element := Default_Substitute)
      return Ada.Streams.Stream_Element_Array;

private

   procedure Decode_No_Check (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Error_Status);

   procedure Encode_No_Check (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Error_Status);

end System.Native_Encoding.Generic_Strings;
