package body Ada.Strings.UTF_Encoding.Strings is

   function Add_Or_Remove_BOM is
      new Generic_Add_Or_Remove_BOM (Character, String, BOM_8);

   function Encode (
      Item : String;
      Output_Scheme : Encoding_Scheme;
      Output_BOM : Boolean := False)
      return UTF_String is
   begin
      return Conversions.Convert (Item, UTF_8, Output_Scheme, Output_BOM);
   end Encode;

   function Encode (
      Item : String;
      Output_BOM : Boolean := False)
      return UTF_8_String
      renames Add_Or_Remove_BOM;

   function Decode (
      Item : UTF_String;
      Input_Scheme : Encoding_Scheme)
      return String is
   begin
      return Conversions.Convert (Item, Input_Scheme, UTF_8, False);
   end Decode;

   function Decode (Item : UTF_8_String) return String is
   begin
      return Add_Or_Remove_BOM (Item, False);
   end Decode;

   function Decode (Item : UTF_16_Wide_String) return String is
   begin
      return Conversions.Convert (Item, False);
   end Decode;

   function Decode (Item : UTF_32_Wide_Wide_String) return String is
   begin
      return Conversions.Convert (Item, False);
   end Decode;

end Ada.Strings.UTF_Encoding.Strings;
