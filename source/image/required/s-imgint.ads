pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_Int is
   pragma Pure;

   --  required for Integer'Image by compiler (s-imgint.ads)
   procedure Image_Integer (
      V : Integer;
      S : in out String;
      P : out Natural);

end System.Img_Int;
