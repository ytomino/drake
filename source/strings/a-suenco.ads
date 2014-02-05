pragma License (Unrestricted);
--  Ada 2012
package Ada.Strings.UTF_Encoding.Conversions is
   pragma Pure;

   --  Conversions between various encoding schemes
   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String;

   --  function from binary to 8 is missing, use from binary to binary

   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String;

   --  extended
   --  This function convets from binary to 32
   function Convert (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String;

   --  function from 8 to binary is missing, use from binary to binary

   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String;

   --  extended
   --  This function convets from 8 to 32
   function Convert (
      Item : UTF_8_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String;

   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String;

   --  extended
   --  This function convets from 16 to 32
   function Convert (
      Item : UTF_16_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_32_Wide_Wide_String;

   --  extended
   --  This function convets from 32 to binary
   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String;

   --  extended
   --  This function convets from 32 to 8
   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_8_String;

   --  extended
   --  This function convets from 32 to 16
   function Convert (
      Item : UTF_32_Wide_Wide_String;
      Output_BOM : Boolean := False)
      return UTF_16_Wide_String;

end Ada.Strings.UTF_Encoding.Conversions;
