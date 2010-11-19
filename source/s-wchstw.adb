with System.UTF_Conversions;
package body System.WCh_StW is
   pragma Suppress (All_Checks);

   procedure String_To_Wide_String (
      S : String;
      R : out Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method)
   is
      pragma Unreferenced (EM);
      Error : Boolean;
   begin
      UTF_Conversions.UTF_8_To_UTF_16 (S, R, L, Error);
   end String_To_Wide_String;

   procedure String_To_Wide_Wide_String (
      S : String;
      R : out Wide_Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method)
   is
      pragma Unreferenced (EM);
      Error : Boolean;
   begin
      UTF_Conversions.UTF_8_To_UTF_32 (S, R, L, Error);
   end String_To_Wide_Wide_String;

end System.WCh_StW;
