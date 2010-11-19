pragma License (Unrestricted);
--  implementation package required by compiler
package System.WCh_StW is
   pragma Pure;

   --  (s-wchcon.ads)
   type WC_Encoding_Method is range 1 .. 6;

   --  required for T'Wide_Image by compiler (s-wchstw.ads)
   procedure String_To_Wide_String (
      S : String;
      R : out Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method);

   --  required for T'Wide_Wide_Image by compiler (s-wchstw.ads)
   procedure String_To_Wide_Wide_String (
      S : String;
      R : out Wide_Wide_String;
      L : out Natural;
      EM : WC_Encoding_Method);

end System.WCh_StW;
