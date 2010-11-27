package body System.Img_Real is
   pragma Suppress (All_Checks);

   procedure Image_Ordinary_Fixed_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Aft : Natural) is
   begin
      raise Program_Error;
   end Image_Ordinary_Fixed_Point;

   procedure Image_Floating_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Digs : Natural) is
   begin
      raise Program_Error;
   end Image_Floating_Point;

end System.Img_Real;
