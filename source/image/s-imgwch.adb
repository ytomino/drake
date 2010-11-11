package body System.Img_WChar is

   procedure Image_Wide_Character (
      V : Wide_Character;
      S : in out String;
      P : out Natural;
      Ada_2005 : Boolean) is
   begin
      raise Program_Error;
   end Image_Wide_Character;

   procedure Image_Wide_Wide_Character (
      V : Wide_Wide_Character;
      S : in out String;
      P : out Natural) is
   begin
      raise Program_Error;
   end Image_Wide_Wide_Character;

end System.Img_WChar;
