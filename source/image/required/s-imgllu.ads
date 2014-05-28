pragma License (Unrestricted);
with System.Unsigned_Types;
--  implementation package required by compiler
package System.Img_LLU is
   pragma Pure;

   --  required for Modular'Image by compiler (s-imgllu.ads)
   procedure Image_Long_Long_Unsigned (
      V : Unsigned_Types.Long_Long_Unsigned;
      S : in out String;
      P : out Natural);

end System.Img_LLU;
