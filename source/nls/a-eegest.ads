pragma License (Unrestricted);
--  extended unit
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
package Ada.Environment_Encoding.Generic_Strings is
   --  Encoding/decoding between strings and character sequences in
   --    Ada.Streams.Stream_Element_Array.
   pragma Preelaborate;

   --  decoder

   type Decoder is new Converter;

   function From (Id : Encoding_Id) return Decoder;

   --  decode subsequence
   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : Boolean;
      Status : out Subsequence_Status_Type);

   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Status : out Continuing_Status_Type);

   procedure Decode (
      Object : Decoder;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Finishing_Status_Type);

   --  decode all character sequence
   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Status_Type);

   --  decode all character sequence with substitute
   procedure Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array;
      Last : out Streams.Stream_Element_Offset;
      Out_Item : out String_Type;
      Out_Last : out Natural;
      Finish : True_Only;
      Status : out Substituting_Status_Type);

   function Decode (
      Object : Decoder;
      Item : Streams.Stream_Element_Array)
      return String_Type;

   --  encoder

   type Encoder is new Converter;

   function To (Id : Encoding_Id) return Encoder;

   --  encode subsequence
   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : Boolean;
      Status : out Subsequence_Status_Type);

   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Status : out Continuing_Status_Type);

   procedure Encode (
      Object : Encoder;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Finishing_Status_Type)
      renames Convert; -- inherited

   --  encode all character sequence
   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Status_Type);

   --  encode all character sequence with substitute
   procedure Encode (
      Object : Encoder;
      Item : String_Type;
      Last : out Natural;
      Out_Item : out Streams.Stream_Element_Array;
      Out_Last : out Streams.Stream_Element_Offset;
      Finish : True_Only;
      Status : out Substituting_Status_Type);

   function Encode (
      Object : Encoder;
      Item : String_Type)
      return Streams.Stream_Element_Array;

end Ada.Environment_Encoding.Generic_Strings;
