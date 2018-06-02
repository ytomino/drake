with System.Formatting.Decimal;
package body System.Img_Dec is

   procedure Image_Decimal (
      V : Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer)
   is
      Fore_Last : Natural;
   begin
      Formatting.Decimal.Image (
         Long_Long_Integer (V),
         S,
         Fore_Last,
         P,
         Scale,
         Aft_Width => Integer'Max (1, Scale));
   end Image_Decimal;

end System.Img_Dec;
