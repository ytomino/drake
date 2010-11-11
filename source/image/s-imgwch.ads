pragma License (Unrestricted);
--  implementation package required by compiler
package System.Img_WChar is
   pragma Pure;

   --  required for Wide_Character'Image by compiler (s-imgwch.ads)
   procedure Image_Wide_Character (
      V : Wide_Character;
      S : in out String;
      P : out Natural;
      Ada_2005 : Boolean);

   --  required for Wide_Wide_Character'Image by compiler (s-imgwch.ads)
   procedure Image_Wide_Wide_Character (
      V : Wide_Wide_Character;
      S : in out String;
      P : out Natural);

end System.Img_WChar;
