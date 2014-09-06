pragma License (Unrestricted);
--  generic implementation of Ada.Strings.UTF_Encoding.Strings
generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   Expanding_From_8 : Positive;
   Expanding_From_16 : Positive;
   Expanding_From_32 : Positive;
   Expanding_To_8 : Positive;
   Expanding_To_16 : Positive;
   Expanding_To_32 : Positive;
   with procedure Get (
      Item : String_Type;
      Last : out Natural;
      Value : out Wide_Wide_Character;
      Is_Illegal_Sequence : out Boolean);
   with procedure Put (
      Value : Wide_Wide_Character;
      Item : out String_Type;
      Last : out Natural);
package Ada.Strings.UTF_Encoding.Generic_Strings is
   pragma Pure;

   --  Encoding / decoding between String_Type and various encoding schemes
   function Encode (
      Item : String_Type;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String;

   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_8_String;

   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String;

   --  extended
   function Encode (
      Item : String_Type;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String;

   function Decode (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme)
      return String_Type;

   function Decode (Item : UTF_8_String) return String_Type;

   function Decode (Item : UTF_16_Wide_String) return String_Type;

   --  extended
   function Decode (Item : UTF_32_Wide_Wide_String) return String_Type;

end Ada.Strings.UTF_Encoding.Generic_Strings;
