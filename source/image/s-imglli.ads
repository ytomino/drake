pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_LLI is
   pragma Pure;

   --  required for Long_Long_Integer'Image by compiler (s-imgint.ads)
   procedure Image_Long_Long_Integer (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural);

end System.Img_LLI;
