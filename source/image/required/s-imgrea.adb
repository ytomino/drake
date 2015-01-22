with System.Formatting.Fixed;
with System.Formatting.Float;
package body System.Img_Real is
   pragma Suppress (All_Checks);

   procedure Image_Ordinary_Fixed_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Aft : Natural) is
   begin
      Formatting.Fixed.Image (
         V,
         S,
         P,
         Aft_Width => Aft);
   end Image_Ordinary_Fixed_Point;

   procedure Image_Floating_Point (
      V : Long_Long_Float;
      S : in out String;
      P : out Natural;
      Digs : Natural) is
   begin
      Formatting.Float.Image (
         V,
         S,
         P,
         Aft_Width => Digs - 1);
   end Image_Floating_Point;

end System.Img_Real;
