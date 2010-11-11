pragma License (Unrestricted);
--  Ada 2012
package Ada.Strings.UTF_Encoding.Conversions is
   pragma Pure;

   --  Conversions between various encoding schemes
--  function Convert (
--   Item : UTF_String;
--   Input_Scheme : Encoding_Scheme;
--   Output_Scheme : Encoding_Scheme;
--   Output_BOM : Boolean := False)
--   return UTF_String;

--  function Convert (
--    Item : UTF_String;
--    Input_Scheme : Encoding_Scheme;
--    Output_BOM : Boolean := False)
--    return UTF_16_Wide_String;

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String;

--  function Convert (
--    Item : UTF_16_Wide_String;
--    Output_Scheme : Encoding_Scheme;
--    Output_BOM : Boolean := False)
--    return UTF_String;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String;

   --  extended

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String;

   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String;

end Ada.Strings.UTF_Encoding.Conversions;
