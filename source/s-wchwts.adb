with System.UTF_Conversions.From_16_To_8;
with System.UTF_Conversions.From_32_To_8;
package body System.WCh_WtS is
   pragma Suppress (All_Checks);

   function Wide_String_To_String (
      S : Wide_String;
      EM : WC_Encoding_Method)
      return String
   is
      pragma Unreferenced (EM);
      R : String (S'First .. S'First - 1 + 3 * S'Length);
      Last : Natural;
   begin
      UTF_Conversions.From_16_To_8.Convert (S, R, Last);
      return R (R'First .. Last);
   end Wide_String_To_String;

   function Wide_Wide_String_To_String (
      S : Wide_Wide_String;
      EM : WC_Encoding_Method) return String
   is
      pragma Unreferenced (EM);
      R : String (S'First .. S'First - 1 + 6 * S'Length);
      Last : Natural;
   begin
      UTF_Conversions.From_32_To_8.Convert (S, R, Last);
      return R (R'First .. Last);
   end Wide_Wide_String_To_String;

end System.WCh_WtS;
