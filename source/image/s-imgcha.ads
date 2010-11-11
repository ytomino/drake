pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_Char is
   pragma Pure;

   --  required for Character'Image by compiler (s-imgcha.ads)
   procedure Image_Character (
      V : Character;
      S : in out String;
      P : out Natural);

end System.Img_Char;
