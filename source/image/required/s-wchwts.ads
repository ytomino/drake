pragma License (Unrestricted);
--  implementation unit required by compiler
package System.WCh_WtS is
   pragma Pure;

   --  (s-wchcon.ads)
   type WC_Encoding_Method is range 1 .. 6;

   --  required for T'Wide_Value by compiler (s-wchwts.ads)
   function Wide_String_To_String (
      S : Wide_String;
      EM : WC_Encoding_Method)
      return String;

   --  required for T'Wide_Wide_Value by compiler (s-wchwts.ads)
   function Wide_Wide_String_To_String (
      S : Wide_Wide_String;
      EM : WC_Encoding_Method)
      return String;

end System.WCh_WtS;
