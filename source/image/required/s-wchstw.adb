with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_8_To_32;
package body System.WCh_StW is

   procedure String_To_Wide_String (
      S : String;
      R : out Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method)
   is
      pragma Unreferenced (EM);
   begin
      UTF_Conversions.From_8_To_16.Convert (S, R, L);
   end String_To_Wide_String;

   procedure String_To_Wide_Wide_String (
      S : String;
      R : out Wide_Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method)
   is
      pragma Unreferenced (EM);
   begin
      UTF_Conversions.From_8_To_32.Convert (S, R, L);
   end String_To_Wide_Wide_String;

end System.WCh_StW;
