pragma License (Unrestricted);
--  Ada 2012
with Ada.Strings.UTF_Encoding.Conversions;
package Ada.Strings.UTF_Encoding.Strings is
   pragma Pure;

   --  Encoding / decoding between String and various encoding schemes
--  function Encode (
--    Item : String;
--    Output_Scheme : Encoding_Scheme;
--    Output_BOM : Boolean := False)
--    return UTF_String;

--  function Encode (
--    Item : String;
--    Output_BOM : Boolean := False)
--    return UTF_8_String;

   function Encode (
      Item : String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String
      renames Conversions.Convert;

   --  extended
   function Encode (
      Item : String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String
      renames Conversions.Convert;

--  function Decode (
--    Item : UTF_String;
--    Input_Scheme : Encoding_Scheme)
--    return String;

--  function Decode (Item : UTF_8_String) return String;

--  function Decode (Item : UTF_16_Wide_String) return String;

end Ada.Strings.UTF_Encoding.Strings;
