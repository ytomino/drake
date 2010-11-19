package body System.Img_Char is
   pragma Suppress (All_Checks);

   procedure Image_Character (
      V : Character;
      S : in out String;
      P : out Natural) is
   begin
      raise Program_Error;
   end Image_Character;

end System.Img_Char;
