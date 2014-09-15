with System.Formatting.Decimal;
package body System.Img_LLD is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Decimal (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer) is
   begin
      Formatting.Decimal.Image (
         V,
         S,
         P,
         Scale,
         Aft_Width => Integer'Max (1, Scale));
   end Image_Long_Long_Decimal;

end System.Img_LLD;
