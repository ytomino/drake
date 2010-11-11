pragma License (Unrestricted);
--  Ada 2012
private with Ada.Strings.UTF_Encoding.Conversions;
package Ada.Strings.UTF_Encoding.Wide_Strings is
   pragma Pure;

   --  Encoding / decoding between Wide_String and various encoding schemes
--  function Encode (
--    Item : Wide_String;
--    Output_Scheme : Encoding_Scheme;
--    Output_BOM : Boolean := False)
--    return UTF_String;

   function Encode (
      Item : Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String;

--  function Encode (
--    Item : Wide_String;
--    Output_BOM : Boolean := False)
--    return UTF_16_Wide_String;

--  function Decode (
--    Item : UTF_String;
--    Input_Scheme : Encoding_Scheme)
--    return Wide_String;

--  function Decode (Item : UTF_8_String) return Wide_String;

--  function Decode (Item : UTF_16_Wide_String) return Wide_String;

private

   function Encode (
      Item : Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String
      renames Conversions.Convert;

end Ada.Strings.UTF_Encoding.Wide_Strings;
