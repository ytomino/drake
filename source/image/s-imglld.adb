package body System.Img_LLD is
   pragma Suppress (All_Checks);

   procedure Image_Long_Long_Decimal (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural;
      Scale : Integer) is
   begin
      raise Program_Error;
   end Image_Long_Long_Decimal;

end System.Img_LLD;
