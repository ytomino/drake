package body System.Img_LLI is

   procedure Image_Long_Long_Integer (
      V : Long_Long_Integer;
      S : in out String;
      P : out Natural) is
   begin
      raise Program_Error;
   end Image_Long_Long_Integer;

end System.Img_LLI;
