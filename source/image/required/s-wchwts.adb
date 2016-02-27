with System.UTF_Conversions.From_16_To_8;
with System.UTF_Conversions.From_32_To_8;
package body System.WCh_WtS is

   function Wide_String_To_String (
      S : Wide_String;
      EM : WC_Encoding_Method)
      return String
   is
      pragma Unreferenced (EM);
   begin
      return UTF_Conversions.From_16_To_8.Convert (S);
   end Wide_String_To_String;

   function Wide_Wide_String_To_String (
      S : Wide_Wide_String;
      EM : WC_Encoding_Method)
      return String
   is
      pragma Unreferenced (EM);
   begin
      return UTF_Conversions.From_32_To_8.Convert (S);
   end Wide_Wide_String_To_String;

end System.WCh_WtS;
