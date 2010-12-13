with System.Formatting.Decimal_Image;
package body System.Img_LLD is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Decimal (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer) is
   begin
      System.Formatting.Decimal_Image (
         S,
         P,
         V,
         Scale,
         Aft_Width => Integer'Max (1, Scale));
   end Image_Long_Long_Decimal;

end System.Img_LLD;
