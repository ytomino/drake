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

   --  decode subsequence
   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Status_Type);

   --  finish decoding and receive remaindered sequence
   procedure Decode (
      Object : Decoder;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Finishing_Status_Type);

   --  decode all character sequence with substitute,
   --    and stop if Out_Item is not large enough
   procedure Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Substituting_Status_Type);

   function Decode (
      Object : Decoder;
      Item : Ada.Streams.Stream_Element_Array)
      return String_Type;

   --  encoder

   type Encoder is new Converter;

   function To (Id : Encoding_Id) return Encoder;

   --  encode subsequence
   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Status_Type);

   --  finish encoding and receive remaindered sequence
   procedure Encode (
      Object : Encoder;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Finishing_Status_Type)
      renames Convert; -- inherited

   --  encode all character sequence with substitute,
   --    and stop if Out_Item is not large enough
   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Ada.Streams.Stream_Element_Array;
      Out_Last : out Ada.Streams.Stream_Element_Offset;
      Status : out Substituting_Status_Type);

   function Encode (
      Object : Encoder;
      Item : String_Type)
      return Ada.Streams.Stream_Element_Array;

end System.Native_Encoding.Generic_Strings;
